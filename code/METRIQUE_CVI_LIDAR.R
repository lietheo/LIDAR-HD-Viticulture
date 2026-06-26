library(sf)
library(terra)

# 1. Chargement des données
parc  <- st_read("C:/Users/tliegeon/Desktop/POUR_CARTE_TEST/output_gironde/metriques_globales_vignes_topo.gpkg")

parc <- parc |> 
  dplyr::filter(candidate_vigne == TRUE)


lidar <- st_read("C:/Users/tliegeon/Desktop/Hilal_Exemple_commune_entiere/Inventaire_LidarHD_Gironde.gpkg")
cvi   <- st_read("C:/Users/tliegeon/Desktop/lidar/shape/Vigne_SpGironde_l93.shp")

# 2. Fonction d'attribution des dates LiDAR
attribuer_dates_lidar <- function(parcelles, dalles) {
  inter <- st_join(parcelles[, "IDU"], dalles[, c("annee", "mois")], join = st_intersects)
  df <- st_drop_geometry(inter)
  df <- df[!is.na(df$annee), ]
  
  if (nrow(df) == 0) {
    if (!"dates_lidar" %in% names(parcelles)) {
      parcelles$dates_lidar <- NA_character_
    }
    return(parcelles)
  }
  
  df$date_iso <- sprintf("%04d-%02d", df$annee, df$mois)
  
  dates_par_parcelle <- aggregate(
    date_iso ~ IDU,
    data = df,
    FUN = function(x) paste(sort(unique(x)), collapse = " ; ")
  )
  
  if ("dates_lidar" %in% names(parcelles)) {
    parcelles$dates_lidar <- NULL
  }
  
  parcelles_finales <- merge(parcelles, dates_par_parcelle, by = "IDU", all.x = TRUE)
  names(parcelles_finales)[names(parcelles_finales) == "date_iso"] <- "dates_lidar"
  
  return(parcelles_finales)
}

# 3. Fonction de jointure CVI et calcul de l'âge
joindre_cvi <- function(parcelles, cvi) {
  cvi_df <- sf::st_drop_geometry(cvi)
  
  # Nettoyeur robuste pour les formats c("A", "B")
  clean_vec <- function(x) {
    x_str <- as.character(unlist(x))
    x_str <- gsub("c\\(|\\)|\"|'", "", x_str)
    valeurs <- unlist(strsplit(x_str, ","))
    valeurs <- trimws(valeurs)
    valeurs <- valeurs[!is.na(valeurs) & valeurs != ""]
    if (length(valeurs) == 0) return(NA_character_)
    return(paste(sort(unique(valeurs)), collapse = " ; "))
  }
  
  message("Agrégation des données CVI...")
  cvi_clean <- data.frame(IDU = unique(cvi_df$IDU), stringsAsFactors = FALSE)
  
  if ("SUP" %in% names(cvi_df)) {
    sup_agg <- aggregate(SUP ~ IDU, data = cvi_df, FUN = function(x) sum(as.numeric(unlist(x)), na.rm = TRUE))
    cvi_clean <- merge(cvi_clean, sup_agg, by = "IDU", all.x = TRUE)
  }
  
  # CONTENANCE : une seule valeur par parcelle (pas de somme, sinon doublonnée
  # autant de fois qu'il y a de lignes/cépages pour le même IDU)
  if ("CONTENANCE" %in% names(cvi_df)) {
    cont_agg <- aggregate(CONTENANCE ~ IDU, data = cvi_df, 
                          FUN = function(x) unique(as.numeric(unlist(x)))[1])
    cvi_clean <- merge(cvi_clean, cont_agg, by = "IDU", all.x = TRUE)
  }
  
  if ("LIBCEP" %in% names(cvi_df)) {
    cep_agg <- aggregate(LIBCEP ~ IDU, data = cvi_df, FUN = clean_vec)
    cvi_clean <- merge(cvi_clean, cep_agg, by = "IDU", all.x = TRUE)
  }
  
  if ("LIBPRD" %in% names(cvi_df)) {
    cep_prd <- aggregate(LIBPRD ~ IDU, data = cvi_df, FUN = clean_vec)
    cvi_clean <- merge(cvi_clean, cep_prd, by = "IDU", all.x = TRUE)
  }
  
  if ("DATE" %in% names(cvi_df)) {
    date_agg <- aggregate(DATE ~ IDU, data = cvi_df, FUN = clean_vec)
    cvi_clean <- merge(cvi_clean, date_agg, by = "IDU", all.x = TRUE)
  }
  
  noms_finaux <- names(cvi_clean)
  names(cvi_clean)[noms_finaux != "IDU"] <- paste0("CVI_", noms_finaux[noms_finaux != "IDU"])
  
  cols_a_supprimer <- names(cvi_clean)[names(cvi_clean) != "IDU"]
  parcelles <- parcelles[, !(names(parcelles) %in% cols_a_supprimer)]
  
  parcelles_finales <- merge(parcelles, cvi_clean, by = "IDU", all.x = TRUE)
  
  # Logique de calcul de l'âge
  calcul_age <- function(str_lidar, str_cvi) {
    if (is.na(str_lidar) | is.na(str_cvi) | str_lidar == "" | str_cvi == "") return(NA_real_)
    
    annees_lidar <- as.numeric(substr(unlist(strsplit(as.character(str_lidar), " ; ")), 1, 4))
    max_lidar <- max(annees_lidar, na.rm = TRUE)
    
    annees_cvi_all <- as.numeric(unlist(regmatches(str_cvi, gregexpr("[0-9]{4}", str_cvi))))
    
    if (length(annees_cvi_all) == 0) return(NA_real_)
    
    annees_valides <- annees_cvi_all[annees_cvi_all <= max_lidar]
    
    if (length(annees_valides) > 0) {
      return(max_lidar - max(annees_valides))
    } else {
      return(-1)
    }
  }
  
  if ("dates_lidar" %in% names(parcelles_finales) & "CVI_DATE" %in% names(parcelles_finales)) {
    parcelles_finales$vigne_age <- mapply(calcul_age, 
                                          parcelles_finales$dates_lidar, 
                                          parcelles_finales$CVI_DATE)
  }
  
  return(parcelles_finales)
}

# 4. Exécution du traitement
parc <- attribuer_dates_lidar(parc, lidar)
parc <- joindre_cvi(parc, cvi)

# 5. Calcul du canopy_cover normalisé par la surface déclarée (CVI)
parc <- parc |>
  dplyr::mutate(
    canopy_cover_normalise = dplyr::if_else(
      !is.na(CVI_CONTENANCE) & CVI_CONTENANCE > 0,
      (canopy_cover * surface_parcelle) / CVI_CONTENANCE,
      NA_real_
    )
  )

st_write(parc, "C:/Users/tliegeon/Desktop/POUR_CARTE_TEST/output_gironde/metriques_globales_vignes_all.gpkg", delete_dsn = TRUE)