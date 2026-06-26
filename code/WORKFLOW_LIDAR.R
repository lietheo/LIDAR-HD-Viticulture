setwd("C:/Users/tliegeon/Desktop/POUR_CARTE_TEST")

packages <- c(
  "sf", "osmdata", "lidR", "terra", "fs",
  "data.table", "dplyr", "giscoR", 
  "landscapemetrics", "exactextractr", "tidyr", "lwgeom"
)

install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p)
    }
  }
}

install_if_missing(packages)

# Empêche R de transformer automatiquement les chaînes de caractères + évite la notation scientifique
options(stringsAsFactors = FALSE, scipen = 999)
# Repart d’un environnement propre,
rm(list = ls())

suppressPackageStartupMessages({
  library(sf)
  library(osmdata)
  library(lidR)
  library(terra)
  library(fs)
  library(data.table)
  library(dplyr)
  library(giscoR)
  library(exactextractr)
  library(tidyr)
  library(lwgeom)
})

######################################

## 0.2 PARAMÈTRES GÉNÉRAUX
# Dossiers de travail LiDAR
dir_tmp <- "./tmp_lidar"
dir_out <- "./output_gironde"

# Chargement direct des fichiers créés en Python
lidar <- st_read("Inventaire_LidarHD_Gironde.gpkg", quiet = TRUE) |> st_transform(2154) # Inventaire géométrique des dalles du dep

dico_brute <- st_read("PARCELLE.SHP", quiet = TRUE) |> st_transform(2154) # Inventaire de l'ensemble des parcelles du département

dossier_cosia <- "COSIA_1-0__GPKG_LAMB93_D033_2024-01-01"

dir_create(dir_tmp, recurse = TRUE)
dir_create(dir_out, recurse = TRUE)

# Résolution des rasters
res_dtm <- 0.5
res_chm <- 0.5

# Seuils de hauteur pour la vigne probable
h_min <- 0.30
h_max <- 2.00

# Buffer autour des communes pour limiter les effets de bord
comm_buffer_m <- 100

# Sortie préparation
dep_commune <- "D_33_communes.gpkg"
dep_parcelle <- "D33_parcelles.gpkg"
vigne_osm <- "vignes_osm.gpkg"
vigne_cosia <- "vignes_cosia.gpkg"
vigne_parcelle_dico <- "vignes_parcelles.gpkg"

#API
url_gisco <- "https://gisco-services.ec.europa.eu/distribution/v2/lau/geojson/LAU_RG_01M_2021_4326.geojson"

######################################

# PREPARATION

# ---------------------------------------------------------------------------
# 1.Communes
# ---------------------------------------------------------------------------

if (!file.exists(dep_commune)) {
  cat("Téléchargement des communes depuis l'API GISCO...\n")
  
  vcom_full <- st_read(url_gisco, quiet = TRUE)
  
  vcom <- vcom_full %>%
    filter(CNTR_CODE == "FR") %>%
    rename(codgeo = LAU_ID, libgeo = LAU_NAME) %>%
    select(codgeo, libgeo) %>%
    st_transform(2154) # Projection en Lambert 93
  
  vcom33 <- vcom %>% filter(grepl("^33", codgeo))
  
  st_write(vcom33, dep_commune, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
} else {
  vcom33 <- st_read(dep_commune, quiet = TRUE)
}

cat("Communes du 33 chargées :", nrow(vcom33), "\n")

# ---------------------------------------------------------------------------
# 2 Parcelles cadastrales
# ---------------------------------------------------------------------------

if (!file.exists(dep_parcelle)) {
  cat("Création du GeoPackage à partir de dico_brute...\n")
  vpar33 <- dico_brute 
  
  cat("Sauvegarde en GeoPackage...\n")
  st_write(vpar33, dep_parcelle, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
} else {
  cat("Chargement des parcelles depuis le GeoPackage local...\n")
  vpar33 <- st_read(dep_parcelle, quiet = TRUE)
}

# ---------------------------------------------------------------------------
# 3 Traitement des vignes officielles (COSIA 2024)
# ---------------------------------------------------------------------------

lire_cosia_vigne <- function(dossier) {
  cat("Lecture des tuiles COSIA (classe vigne)...\n")
  fichiers <- list.files(dossier, pattern = "\\.gpkg$", recursive = TRUE, full.names = TRUE)
  
  if (length(fichiers) == 0) stop("Aucun fichier .gpkg trouvé.")
  
  lst <- list()
  for (i in seq_along(fichiers)) {
    f <- fichiers[i]
    cat(sprintf("[%d/%d] %s\n", i, length(fichiers), basename(f)))
    
    tryCatch({
      # Lecture du fichier et filtrage sur le numéro 11 (vigne)
      couches <- st_layers(f)$name
      x <- st_read(f, layer = couches[1], quiet = TRUE)
      x <- x %>% filter(numero == "11")
      
      if (nrow(x) > 0) {
        lst[[length(lst) + 1]] <- x
      }
    }, error = function(e) {
      message(sprintf("Avertissement : Erreur sur %s (%s)", basename(f), e$message))
    })
  }
  
  cat("Fusion des tuiles COSIA...\n")
  cosia_vigne <- bind_rows(lst)
  cosia_vigne <- st_make_valid(cosia_vigne)
  
  return(cosia_vigne)
}

cat("Préparation des référentiels...\n")

if (!file.exists(vigne_cosia)) {
  cat("Lecture des vignes COSIA sur la Gironde...\n")
  
  cosia_vigne <- lire_cosia_vigne(dossier_cosia)
  cosia_vigne <- st_transform(cosia_vigne, st_crs(vpar33))
  
  st_write(cosia_vigne, vigne_cosia, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
} else {
  cosia_vigne <- st_read(vigne_cosia, quiet = TRUE)
}

# ---------------------------------------------------------------------------
# 4 Vignes OSM
# ---------------------------------------------------------------------------

if (!file.exists(vigne_osm)) {
  cat("Téléchargement des vignobles OSM sur la Gironde...\n")
  
  # Reprojection en WGS 84 (EPSG:4326) requise par l'API OpenStreetMap
  vcom33_wgs <- st_transform(vcom33, 4326)
  
  # Extraction de la "bounding box" (emprise rectangulaire globale de la Gironde)
  emprise_gironde <- st_bbox(vcom33_wgs)
  
  # Requête Overpass via osmdata pour la clé landuse=vineyard
  requete_osm <- opq(bbox = emprise_gironde) %>%
    add_osm_feature(key = "landuse", value = "vineyard") %>%
    osmdata_sf()
  
  # Filtrage pour ne conserver que les surfaces (Polygones et MultiPolygones)
  osm_vigne <- bind_rows(requete_osm$osm_polygons, requete_osm$osm_multipolygons)
  
  if (nrow(osm_vigne) > 0) {
    # Correction des géométries potentiellement invalides issues d'OSM
    osm_vigne <- st_make_valid(osm_vigne)
    
    # Nettoyage préventif : on convertit toutes les colonnes attributaires en texte 
    # pour éviter que les listes renvoyées ne fassent planter l'écriture du GeoPackage
    osm_vigne <- osm_vigne %>% mutate(across(where(~!inherits(.x, "sfc")), as.character))
    
    # Reprojection dans le même système de coordonnées que le parcellaire de référence
    osm_vigne <- st_transform(osm_vigne, st_crs(vpar33))
    
    # Découpage stricte
    contour_gironde <- st_union(vcom33) 
    osm_vigne <- st_intersection(osm_vigne, contour_gironde)
    
    # Sauvegarde locale au format GeoPackage
    st_write(osm_vigne, vigne_osm, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
  }
} else {
  osm_vigne <- st_read(vigne_osm, quiet = TRUE)
}

# ---------------------------------------------------------------------------
# 5 Dictionnaire des parcelles candidates
# ---------------------------------------------------------------------------

if (!file.exists(vigne_parcelle_dico)) {
  message("Construction du dictionnaire parcellaire...")
  
  # --- Intersections OSM ---
  message("Calcul des intersections brutes avec OSM...")
  ix_osm <- st_intersection(vpar33, st_geometry(osm_vigne))
  ix_osm$area_osm <- as.numeric(st_area(ix_osm))
  
  tab_osm <- ix_osm |>
    st_drop_geometry() |>
    group_by(IDU) |>
    summarise(surf_osm_vigne = sum(area_osm), .groups = "drop")
  
  # --- Intersections COSIA ---
  message("Calcul des intersections brutes avec COSIA...")
  ix_cosia <- st_intersection(vpar33, st_geometry(cosia_vigne))
  ix_cosia$area_cosia <- as.numeric(st_area(ix_cosia))
  
  tab_cosia <- ix_cosia |>
    st_drop_geometry() |>
    group_by(IDU) |>
    summarise(surf_cosia_vigne = sum(area_cosia), .groups = "drop")
  
  # --- Dictionnaire final ---
  message("Assemblage du dictionnaire final...")
  
  dico <- vpar33 |>
    mutate(surface_parcelle = round(as.numeric(st_area(vpar33)), 3)) |>
    left_join(tab_osm, by = "IDU") |>
    left_join(tab_cosia, by = "IDU") |>
    mutate(
      surf_osm_vigne = round(as.numeric(coalesce(surf_osm_vigne, 0)), 3),
      surf_cosia_vigne = round(as.numeric(coalesce(surf_cosia_vigne, 0)), 3),
      part_osm = round(as.numeric(surf_osm_vigne / surface_parcelle), 3),
      part_cosia = round(as.numeric(surf_cosia_vigne / surface_parcelle), 3),
      presence_osm = part_osm > 0,
      presence_cosia = part_cosia > 0,
      candidate_vigne = (part_osm >= 0.05) | (part_cosia >= 0.05),
      source_candidate = case_when(
        part_osm >= 0.05 & part_cosia >= 0.05 ~ "osm+cosia",
        part_osm >= 0.05 ~ "osm",
        part_cosia >= 0.05 ~ "cosia",
        TRUE ~ "none"
      ),
      part_cosia_pct = round(as.numeric(100 * part_cosia), 3),
      part_osm_pct = round(as.numeric(100 * part_osm), 3),
      IDU = as.character(IDU)
    )
  
  st_write(dico, vigne_parcelle_dico, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
} else {
  dico <- st_read(vigne_parcelle_dico, quiet = TRUE) |>
    mutate(IDU = as.character(IDU))
}

######################################

# TRAITEMENT (Boucles)

# -------------------------------------------------------------------------
# 1. TÉLÉCHARGEMENT DES DALLES LIDAR
# -------------------------------------------------------------------------

# Configuration globale pour stabiliser la connexion avec les serveurs IGN
options(timeout = 600) # Augmentation à 10 minutes pour éviter les coupures de temps
options(download.file.method = "curl") # Passage de libcurl à curl pour permettre les arguments système
options(download.file.extra = "--retry 3 --retry-delay 5") # active 3 relances automatiques en cas de coupure

download_tile <- function(url, dest_dir) {
  f <- file.path(dest_dir, basename(url))
  
  if (!file.exists(f)) {
    success <- FALSE
    attempts <- 0
    max_attempts <- 3
    
    while (!success && attempts < max_attempts) {
      attempts <- attempts + 1
      
      tryCatch({
        # Tentative de téléchargement
        download.file(url, f, mode = "wb", quiet = TRUE)
        success <- TRUE
      }, error = function(e) {
        message("Échec du téléchargement (tentative ", attempts, "/", max_attempts, ") : ", basename(url))
        # Suppression du fichier partiel corrompu s'il a été créé
        if (file.exists(f)) file.remove(f)
        Sys.sleep(5) # Pause de 5 secondes avant de relancer
      })
    }
    
    if (!success) {
      stop("Impossible de télécharger le fichier après ", max_attempts, " tentatives : ", url)
    }
  }
  
  return(f)
}

# -------------------------------------------------------------------------
# 2) MASQUES VIGNE
# -------------------------------------------------------------------------

# Masque brut basé sur la hauteur
build_mask <- function(chm, h_min, h_max) {
  ifel(chm >= h_min & chm <= h_max, 1, 0)
}

# Nettoyage morphologique

# Reclassification simple en 0/1 (bianaire)
clean_mask <- function(mask, com_vect) {
  m <- classify(mask, matrix(c(-Inf, 0.5, 0,
                               0.5, Inf, 1), 3, byrow = TRUE))
  
  # Comptage des voisins actifs (fenêtre 3x3)
  n9 <- focal(m, matrix(1, 3, 3), sum, na.rm = TRUE)
  m <- ifel(n9 >= 6, 1, 0) # Seuil : au moins 6 voisins actifs
  
  # Nettoyage morphologique (fermeture)
  m <- focal(m, matrix(1, 3, 3), max, na.rm = TRUE) # Dilatation
  m <- focal(m, matrix(1, 3, 3), min, na.rm = TRUE) # Érosion
  
  # Restriction a la commune (union parcelles)
  m <- mask(m, com_vect)
  
  # Remplacement des NA par 0
  ifel(is.na(m), 0, m)
}

# -------------------------------------------------------------------------
# 3) FONCTION PRETRAITEMENT LIDAR D’UNE COMMUNE
# -------------------------------------------------------------------------

process_commune <- function(com_code) {
  
  message("---- ", com_code)
  
  # 1) Extraction de la commune (parcelles) et préparation des géométries
  
  # Parcelles  de la commune
  # parc <- dico |>
  # filter(substr(IDU, 1, 5) == com_code)
  
  # Parcelles CANDIDATES de la commune
  parc <- dico |>
    filter(candidate_vigne, substr(IDU, 1, 5) == com_code)
  if (nrow(parc) == 0) return(NULL)
  
  emprise_parcelles_commune <- st_union(parc) |> st_as_sf()
  
  # Dossier de sortie
  out_dir <- file.path(dir_out, com_code)
  dir_create(out_dir, recurse = TRUE)
  
  tmp_com <- file.path(dir_tmp, com_code)
  dir_create(tmp_com, recurse = TRUE)
  
  # Commune élargie pour éviter les effets de bord (buffer)
  com_buf <- st_buffer(emprise_parcelles_commune, comm_buffer_m)
  
  # Conversion en terra
  com_vect <- vect(emprise_parcelles_commune)
  com_buf_vect <- vect(com_buf)
  parc_vect <- vect(parc)
  
  # 2) Sélection et téléchargement des dalles LiDAR
  # Dalles LiDAR nécessaires (intersection du buffer de la commune)
  id_tiles <- lengths(st_intersects(lidar, com_buf)) > 0
  tiles <- lidar[id_tiles, ]
  
  if (nrow(tiles) == 0) return(NULL)
  
  # Téléchargement LIDAR local (commune) vers le dossier temporaire spécifique
  files <- vapply(tiles$url_laz, function(x) download_tile(x, dest_dir = tmp_com), character(1))
  
  # 3) Construction d’un LAScatalog pour un traitement distribué
  
  ctg <- readLAScatalog (files)
  
  opt_chunk_size(ctg) <- 1000 # taille des blocs de traitement
  opt_chunk_buffer(ctg) <- 30 # zone tampon
  opt_progress(ctg) <- TRUE
  opt_laz_compression(ctg) <- TRUE
  
  # 4) Calcul du MNT (Modèle Numérique de Terrain)
  # MNT sur emprise large avec cache (méthode IDW avec KNN voisin, 10 points   voisins et power 2)
  f_dtm <- file.path(tmp_com, "dtm_full.tif")
  
  if (file.exists(f_dtm)) {
    message("DTM déjà calculé : ", com_code)
    dtm <- rast(f_dtm)
  } else {
    # Modèle de nommage temporaire spécifique au MNT
    opt_output_files(ctg) <- file.path(tmp_com, "dtm_tmp_{XLEFT}_{YBOTTOM}")
    dtm <- rasterize_terrain(
      ctg,
      res = res_dtm,
      algorithm = knnidw(k = 10, p = 2)
    )
    
    if (!hasValues(dtm)) return(NULL)
    
    writeRaster(dtm, f_dtm, overwrite = TRUE)
  }
  
  # 5) Normalisation des hauteurs
  # Normalisation sur emprise large (toutes les dalles de la commune).
  # Avec un LAScatalog, normalize_height() a besoin d’un template de sortie ;
  # le résultat est un nouveau LAScatalog pointant vers les fichiers
  opt_filter(ctg) <- "-keep_class 3 4 5" #AJOUT DU FILTRE : On ne garde que les classes 3 (basse), 4 (moyenne) et 5 (haute veg)
  opt_output_files(ctg) <- file.path(tmp_com, "norm_{XLEFT}_{YBOTTOM}")
  ctg_norm <- normalize_height(ctg, dtm)
  
  # 6) Calcul du CHM (Canopy Height Model)
  
  opt_output_files(ctg_norm) <- file.path(tmp_com, "chm_tmp_{XLEFT}_{YBOTTOM}")
  
  chm <- rasterize_canopy(
    ctg_norm,
    res = res_chm,
    algorithm = p2r(subcircle = 0.2)
  )
  
  if (!hasValues(chm)) return(NULL)
  
  # 7) Construction du masque vigne
  mask_raw <- build_mask(chm, h_min = h_min, h_max = h_max)
  mask_raw <- mask(crop(mask_raw, com_buf_vect), com_buf_vect)
  
  # Nettoyage morphologique sur la commune
  # mask_clean <- clean_mask(mask_raw, com_vect)
  
  # 8) Découpe finale à la commune (parcelles de la commune)
  # Découpe finale à la commune
  chm_com <- mask(crop(chm, com_vect), com_vect)
  mask_raw_com <- mask(crop(mask_raw, com_vect), com_vect)
  # mask_clean_com <- mask(crop(mask_clean, com_vect), com_vect)
  
  # Exports raster
  writeRaster(chm_com, file.path(out_dir, "chm.tif"), overwrite = TRUE)
  writeRaster(mask_raw_com, file.path(out_dir, "vegmask.tif"), overwrite = TRUE)
  # writeRaster(mask_clean_com, file.path(out_dir, "vegclean.tif"), overwrite = TRUE)
  
  # Suppression du répertoire temporaire à la fin du traitement de la commune
  unlink(tmp_com, recursive = TRUE)
}

######################################
# -------------------------------------------------------------------------
# TEST 
# -------------------------------------------------------------------------

# Code INSEE de la commune à traiter
# Saint-Émilion : Le grand plateau calcaire (Prestige, parcelles historiques, AOC ultra-stricte).
# Francs : Les collines de haute altitude (Climat continental, vins blancs, forte proportion de Bio).
# La Rivière : Les falaises et pentes abruptes (Molasse, travail manuel forcé par le relief).
# Sainte-Terre : La plaine alluviale plate (Sables/limons, haute mécanisation, rendements plus hauts).
# Néac : Les terrasses de cailloux (Graves, sols chauds et drainants).
# Les 5 entre Mai et Juin 2021

codes <- "33356" # 33394 St Emilion, Francs 33173 (blanc), La Rivière 33356 (pente), Sainte-Terre 33485 (plat, pas de grand cru), Néac 33302 (pédo intéressante)

for (c in codes) {
  res <- process_commune(c)
  
  # 1. Vérification d'une erreur (ex: échec définitif de téléchargement)
  if (inherits(res, "try-error")) {
    message("Erreur rencontrée pour la commune ", c, ". Passage à la suivante.")
    next 
  }
  
  # 2. Vérification de l'absence de données (ex: aucune parcelle candidate)
  if (is.null(res)) {
    message("Aucune donnée candidate pour la commune ", c, ". Passage à la suivante.")
    next
  }
}

