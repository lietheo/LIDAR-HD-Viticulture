library(sf)
library(terra)
library(dplyr)
library(exactextractr)


# 1. Charger les parcelles candidates
parc <- st_read("C:/Users/tliegeon/Desktop/POUR_CARTE_TEST/output_gironde/metriques_globales_vignes.gpkg") |>
  dplyr::filter(candidate_vigne == TRUE)

# -------------------------------------------------------------------------
# FONCTION CALCUL DES MÉTRIQUES TOPO (5 m uniquement)
# -------------------------------------------------------------------------

process_parcelle_topo <- function(parc) {
  
  # 0. Sécurité géométrique
  parc <- st_make_valid(parc)
  parc <- parc[!st_is_empty(parc), ]
  
  # 1. Charger le raster 5 m
  r5m <- rast("C:/Users/tliegeon/Desktop/propre/topographie_gironde/GIRONDE_TOPO_STACK_5M.tif")
  
  # 2. Extraire les 4 bandes utiles
  # Bande 1 = altitude
  # Bande 2 = slope
  # Bande 3 = aspect
  # Bande 4 = twi
  r_sub <- r5m[[c(1, 2, 3, 4)]]
  r_round <- terra::round(r_sub)
  
  # 3. Extraction en une seule passe
  vals <- exact_extract(
    x = r_round,
    y = parc,
    fun = "majority",
    progress = FALSE
  )
  
  # 4. Affectation des valeurs
  parc$altitude <- vals[[1]]
  parc$slope    <- vals[[2]]
  parc$aspect   <- vals[[3]]
  parc$twi      <- vals[[4]]
  
  return(parc)
}

# 2. Exécution du traitement
parc <- process_parcelle_topo(parc)

# 3. Export
st_write(parc, "C:/Users/tliegeon/Desktop/POUR_CARTE_TEST/output_gironde/metriques_globales_vignes_topo.gpkg", delete_dsn = TRUE)
