# Chargement des bibliothèques requises
library(sf)
library(terra)
library(exactextractr)
library(dplyr)

# ---------------------------------------------------------
# Étape 1 : Paramètres et chargement des données globales
# ---------------------------------------------------------
# Définir le dossier principal qui contient tous les sous-dossiers des communes
dossier_principal <- "C:/Users/tliegeon/Desktop/POUR_CARTE_TEST/output_gironde"

# Chargement du GeoPackage global des parcelles
parcelles <- st_read("C:/Users/tliegeon/Desktop/POUR_CARTE_TEST/vignes_parcelles.gpkg")

# Surface d'un pixel
surface_unitaire_pixel <- 0.25

# Lister tous les sous-dossiers présents dans "output_gironde"
dossiers_communes <- list.dirs(dossier_principal, full.names = TRUE, recursive = FALSE)

# Création d'une liste vide qui va stocker les résultats de chaque boucle
liste_resultats <- list()

# ---------------------------------------------------------
# Étape 2 : La boucle sur chaque commune
# ---------------------------------------------------------
for (dossier in dossiers_communes) {
  
  # Extraire le nom du dossier en cours (ex: 33394_Saint_Emilion")
  nom_dossier <- basename(dossier)
  
  # Extraire les 5 premiers caractères pour récupérer uniquement le code INSEE
  code_insee <- substr(nom_dossier, 1, 5)
  
  # Construire les chemins dynamiques vers les rasters
  chemin_chm <- file.path(dossier, "chm.tif")
  chemin_masque <- file.path(dossier, "vegmask.tif")
  
  # SÉCURITÉ : On vérifie que les rasters existent bien dans ce dossier
  if (file.exists(chemin_chm) && file.exists(chemin_masque)) {
    
    cat("Traitement en cours pour la commune :", code_insee, "...\n")
    
    # 1. Filtrer les parcelles candidates pour cette commune spécifique
    vignes_commune <- parcelles %>%
      filter(substr(IDU, 1, 5) == code_insee) %>%
      filter(candidate_vigne == TRUE)
    
    # S'il n'y a aucune parcelle pour cette commune, on évite un crash et on passe à la suite
    if (nrow(vignes_commune) == 0) {
      cat("  -> Aucune parcelle vigne trouvée. Dossier ignoré.\n")
      next
    }
    
    # 2. Chargement des rasters pour la commune
    chm <- rast(chemin_chm)
    masque <- rast(chemin_masque)
    
    # 3. Masquer le CHM
    chm_vigne <- mask(chm, masque, maskvalues = 0)
    
    # 4. Extraction avec exactextractr (progress=FALSE pour éviter de spammer la console)
    stats <- exact_extract(chm_vigne, vignes_commune, c('mean', 'stdev', 'count'), progress = FALSE)
    
    # 5. Calculs des variables dérivées
    resultats_commune <- vignes_commune %>%
      bind_cols(stats) %>%
      mutate(
        surface_canopee = count * surface_unitaire_pixel,
        canopy_cover = surface_canopee / surface_parcelle,
        code_commune = code_insee
      ) %>%
      rename(
        hmean = mean,
        hstd = stdev
      )
    
    # 6. Ajouter le résultat de cette commune dans la liste
    liste_resultats[[code_insee]] <- resultats_commune
    
  } else {
    # Si les fichiers n'existent pas, on le signale
    cat("Ignoré : Fichiers chm.tif ou vegmask.tif introuvables dans", nom_dossier, "\n")
  }
}

# ---------------------------------------------------------
# Étape 3 : Assemblage et Export du GeoPackage final unique
# ---------------------------------------------------------
cat("\nFusion de tous les résultats en cours...\n")

# Si la liste contient au moins un résultat
if (length(liste_resultats) > 0) {
  
  # Fusion de toutes les communes en un seul objet spatial
  resultats_finaux <- bind_rows(liste_resultats)
  
  # Chemin de sauvegarde du GeoPackage global
  chemin_export <- file.path(dossier_principal, "metriques_globales_vignes.gpkg")
  
  # Exportation
  st_write(resultats_finaux, chemin_export, append = FALSE)
  cat("\nLe GeoPackage global a été sauvegardé ici :\n", chemin_export, "\n")
  
} else {
  cat("\nAucun résultat n'a été calculé (vérifie tes chemins et tes données).\n")
}