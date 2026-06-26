library(sf)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)

##### ACP - METRIQUES STRUCTURELLES - ANALYSE #####

# Forcer l'encodage des caractères pour Windows
Sys.setlocale("LC_ALL", "French")

# 1. Chargement
chemin_fichier <- "C:/Users/tliegeon/Desktop/POUR_CARTE_TEST/output_gironde/metriques_globales_vignes_all.gpkg"
donnees_spatiales <- st_read(chemin_fichier, quiet = TRUE)

# 2. Filtrage strict + Préparation des variables
donnees_finales <- st_drop_geometry(donnees_spatiales) %>%
  # Application des filtres d'exclusion stricts (NOT LIKE '%;%')
  dplyr::filter(
    !grepl(";", CVI_LIBCEP) &
      !grepl(";", CVI_DATE) &
      !grepl(";", CVI_LIBPRD) &
      !is.na(vigne_age) &
      !is.na(CVI_CONTENANCE) & CVI_CONTENANCE > 0
  ) %>%
  dplyr::mutate(
    # --- Orientation cardinale (Intégration des terrains plats -1) ---
    Orientation_Cardinale = case_when(
      aspect == -1                 ~ "Plat",
      aspect >= 315 | aspect < 45  ~ "Nord",
      aspect >= 45  & aspect < 135 ~ "Est",
      aspect >= 135 & aspect < 225 ~ "Sud",
      aspect >= 225 & aspect < 315 ~ "Ouest",
      TRUE                         ~ NA_character_
    ),
    Orientation_Cardinale = factor(Orientation_Cardinale, levels = c("Nord", "Est", "Sud", "Ouest", "Plat")),
    
    # --- Nettoyage et filtrage exclusif des appellations ---
    AOP_clean = toupper(trimws(CVI_LIBPRD)),
    AOP_clean = gsub("CÃ´TES", "COTES", AOP_clean),
    AOP_clean = gsub("CÃ`TES", "COTES", AOP_clean),
    
    APPELLATION = case_when(
      grepl("SAINT-EMILION GRAND CRU|VIN APTE SAINT-EMILION GRAND CRU", AOP_clean) ~ "SAINT-ÉMILION GRAND CRU",
      AOP_clean == "SAINT-EMILION"                  ~ "SAINT-ÉMILION",
      AOP_clean == "POMEROL"                        ~ "POMEROL",
      AOP_clean == "LALANDE-DE-POMEROL"             ~ "LALANDE-DE-POMEROL",
      AOP_clean == "FRONSAC"                        ~ "FRONSAC",
      grepl("FRANCS ROUGE", AOP_clean)                    ~ "CÔTES DE BORDEAUX FRANCS ROUGE",
      AOP_clean == "BORDEAUX ROUGE"                 ~ "BORDEAUX ROUGE",
      TRUE                                          ~ NA_character_ # Exclut toutes les autres AOP
    ),
    
    # --- Cépage (Pures mono-cépages suite au filtre global du point-virgule) ---
    Cepage_Final = trimws(CVI_LIBCEP)
  ) %>%
  # Suppression des lignes n'appartenant pas aux AOP ciblées ou sans orientation valide
  tidyr::drop_na(Orientation_Cardinale, APPELLATION, Cepage_Final)

# 3. Construction du tableau ACP (actives + supplémentaires)
donnees_acp <- donnees_finales %>%
  dplyr::select(
    IDU,
    hmean, hstd, canopy_cover_normalise,              # 1-3 : Variables ACTIVES
    altitude, slope, twi, vigne_age,                  # 4-7 : Variables QUANTI supplémentaires
    Orientation_Cardinale, APPELLATION, Cepage_Final  # 8-10 : Variables QUALI supplémentaires
  ) %>%
  tidyr::drop_na(hmean, hstd, canopy_cover_normalise) %>%
  as.data.frame()

#########################################
# ANALYSE BIVARIÉE EXPLORATOIRE
#########################################

# Variables structurelles LiDAR
metriques_lidar <- donnees_acp %>%
  dplyr::select(
    hmean,
    hstd,
    canopy_cover_normalise
  )

# 1. Relations avec les variables quantitatives

vars_quanti <- donnees_acp %>%
  dplyr::select(
    hmean,
    hstd,
    canopy_cover_normalise,
    altitude,
    slope,
    twi,
    vigne_age
  )

matrice_corr <- cor(
  vars_quanti,
  method = "pearson",
  use = "complete.obs"
)

print("--------------------------------")
print("CORRELATION VARIABLES QUANTITATIVES")
print("--------------------------------")

print(round(matrice_corr,2))

# 2. Relations avec les variables qualitatives

# Fonction ANOVA automatique pour les appéllations
analyse_anova <- function(variable){
  
  modele <- aov(
    as.formula(
      paste(variable, "~ APPELLATION")
    ),
    data = donnees_acp
  )
  
  print(summary(modele))
}


print("--------------------------------")
print("ANOVA : METRIQUES LIDAR ~ APPELLATION")
print("--------------------------------")


analyse_anova("hmean")
analyse_anova("hstd")
analyse_anova("canopy_cover_normalise")

# Cépage
analyse_anova_cepage <- function(variable){
  
  modele <- aov(
    as.formula(
      paste(variable, "~ Cepage_Final")
    ),
    data = donnees_acp
  )
  
  print(summary(modele))
}


print("--------------------------------")
print("ANOVA : METRIQUES LIDAR ~ CEPAGE")
print("--------------------------------")


analyse_anova_cepage("hmean")
analyse_anova_cepage("hstd")
analyse_anova_cepage("canopy_cover_normalise")

# Aspect

analyse_anova_aspect <- function(variable){
  
  modele <- aov(
    as.formula(
      paste(variable, "~ Orientation_Cardinale")
    ),
    data = donnees_acp
  )
  
  print(summary(modele))
}


print("--------------------------------")
print("ANOVA : METRIQUES LIDAR ~ Orientation")
print("--------------------------------")


analyse_anova_aspect("hmean")
analyse_anova_aspect("hstd")
analyse_anova_aspect("canopy_cover_normalise")

###############

# 4. Exécution de l'ACP
res.pca <- PCA(donnees_acp%>% dplyr::select(-IDU),
               quanti.sup = 4:7,
               quali.sup  = 8:10,
               graph = FALSE)

# 5. Visualisations et affichage des résultats
print("--- Part de variance expliquée par axe ---")
print(res.pca$eig)

# Cercle de corrélation des variables (Actives et supplémentaires)
fviz_pca_var(res.pca, repel = TRUE)

# Graphe des individus avec les ellipses de confiance des AOP
fviz_pca_ind(res.pca, habillage = "APPELLATION", geom = "point",
             alpha.ind = 0.15, addEllipses = TRUE, ellipse.level = 0.95)

# Description des axes par les variables
desc <- dimdesc(res.pca, axes = 1:2, proba = 0.05)

print("--- Liaison avec l'axe 1 ---")
print(desc$Dim.1)

print("--- Liaison avec l'axe 2 ---")
print(desc$Dim.2)

#########################################
# CLASSIFICATION ASCENDANTE HIÉRARCHIQUE (CAH)
#########################################
# Chargement des bibliothèques

# 1. Coordonnées ACP (axes 1 et 2)
coords <- res.pca$ind$coord[, 1:2]

# 2. Matrice de distance et CAH (Ward)
dist_mat <- dist(coords)
cah <- hclust(dist_mat, method = "ward.D2")

# Visualisation du dendrogramme
plot(cah, labels = FALSE, main = "Dendrogramme de la CAH (Methode de Ward)", xlab = "", sub = "")

# 3. Découpage en 3 classes
donnees_acp$Classe_CAH <- cutree(cah, k = 3)
donnees_acp$Classe_CAH <- factor(paste0("Classe_", donnees_acp$Classe_CAH))

# 4. Caractérisation morphologique des classes
carac_classes <- donnees_acp %>%
  dplyr::group_by(Classe_CAH) %>%
  dplyr::summarise(
    Effectif = n(),
    hmean = round(mean(hmean, na.rm = TRUE), 2),
    hstd = round(mean(hstd, na.rm = TRUE), 2),
    canopy_cover_normalise = round(mean(canopy_cover_normalise, na.rm = TRUE), 2)
  )

print("--- Profil morphologique moyen des classes ---")
print(as.data.frame(carac_classes))

# 5. Proportion des classes par Appellation
tab_croise <- table(donnees_acp$APPELLATION, donnees_acp$Classe_CAH)
tab_prop <- prop.table(tab_croise, margin = 1) * 100

print("--- Repartition des Appellations dans les classes morphologiques (%) ---")
print(round(tab_prop, 1))

# ---------------------------------------------------------
# 6. EXPORTATION SPATIALE (GEO-PACKAGE)
# ---------------------------------------------------------

# Jointure stricte (inner_join) basée sur l'identifiant fixe IDU
couche_export <- donnees_spatiales %>%
  dplyr::inner_join(
    donnees_acp %>% dplyr::select(IDU, Classe_CAH, APPELLATION, Orientation_Cardinale, Cepage_Final), 
    by = "IDU"
  )

# Exportation du fichier .gpkg
chemin_export_gpkg <- "C:/Users/tliegeon/Desktop/POUR_CARTE_TEST/output_gironde/classification_cah.gpkg"
st_write(couche_export, dsn = chemin_export_gpkg, delete_dsn = TRUE, quiet = TRUE)

print("Exportation du GeoPackage terminée avec succès.")


