# =========================================================================
# 0. INITIALISATION ET CHARGEMENT DES BIBLIOTHÈQUES
# =========================================================================
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(exactextractr)
library(ggplot2)
library(scales)

# =========================================================================
# 1. PARAMÈTRES ET CHEMINS DE FICHIERS
# =========================================================================
# Code INSEE de Saint-Émilion
code_commune <- "33394"

chemin_gpkg <- "C:/Users/tliegeon/Desktop/Hilal_Exemple_commune_entiere/output_gironde/33394/parcelles_commune.gpkg" 
chemin_tif  <- "C:/Users/tliegeon/Desktop/Hilal_Exemple_commune_entiere/output_gironde/33394/vegclean.tif"
chemin_sortie_gpkg <- "C:/Users/tliegeon/Desktop/Hilal_Exemple_commune_entiere/output_gironde/parcelles_resultats.gpkg"
chemin_cvi <- "C:/Users/tliegeon/Desktop/lidar/shape/zone.shp"

# =========================================================================
# 2. IMPORTATION ET PRÉPARATION
# =========================================================================
# Importation des données de base
dico <- st_read(chemin_gpkg, quiet = TRUE)
raster_veg <- rast(chemin_tif)

# Filtrage et nettoyage géométrique des parcelles
parc_com <- dico |> 
  filter(substr(IDU, 1, 5) == code_commune) |>
  st_make_valid()

parc_com <- parc_com[!st_is_empty(parc_com), ]

# Importation du CVI et extraction des IDU
# On ne garde que la colonne IDU et on supprime la géométrie pour alléger la mémoire
CVI <- st_read(chemin_cvi, quiet = TRUE) |> 
  st_drop_geometry() |> 
  select(IDU) |> 
  distinct()

# Création d'un vecteur contenant la liste des IDU considérés comme vigne
liste_idu_CVI <- CVI$IDU

# =========================================================================
# 3. EXTRACTION LIDAR ET CROISEMENT CVI
# =========================================================================
# Extraction de la proportion de végétation
parc_com$proportion_veg <- exact_extract(x = raster_veg, y = parc_com, fun = "mean")

# Calcul des indicateurs dérivés et vérification de la présence dans le CVI via l'IDU
parc_com <- parc_com |>
  mutate(
    presence_lidar = proportion_veg > 0,
    pct_veg = proportion_veg * 100,
    presence_CVI = IDU %in% liste_idu_CVI
  )

# Sauvegarde du fichier spatial enrichi
st_write(parc_com, chemin_sortie_gpkg, delete_dsn = TRUE, quiet = TRUE)

# =========================================================================
# 4. PRÉPARATION DES DONNÉES POUR LA VISUALISATION
# =========================================================================
# Création d'un tableau sans géométrie et filtré pour les graphiques
df_visu <- parc_com |> 
  st_drop_geometry() |> 
  filter(!is.na(presence_lidar), !is.na(candidate_vigne), !is.na(pct_veg), !is.na(presence_CVI))

# Application de l'ordre d'affichage standard (TRUE d'abord)
df_visu$candidate_vigne <- factor(df_visu$candidate_vigne, levels = c("TRUE", "FALSE"))
df_visu$presence_lidar  <- factor(df_visu$presence_lidar,  levels = c("TRUE", "FALSE"))
df_visu$presence_CVI    <- factor(df_visu$presence_CVI,    levels = c("TRUE", "FALSE"))

# =========================================================================
# 5. GÉNÉRATION DES GRAPHIQUES
# =========================================================================

# --- 5.1 Matrice des surfaces (Candidat vs LiDAR) ---
tableau_surfaces <- xtabs(surface_parcelle ~ candidate_vigne + presence_lidar, data = df_visu)
df_surface <- as.data.frame(tableau_surfaces)

df_surface$candidate_vigne <- factor(df_surface$candidate_vigne, levels = c("FALSE", "TRUE"))
df_surface$presence_lidar  <- factor(df_surface$presence_lidar,  levels = c("TRUE", "FALSE"))

p_surface <- ggplot(df_surface, aes(x = presence_lidar, y = candidate_vigne, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = format(round(Freq), big.mark = " ")),
            color = "black", fontface = "bold", size = 4.5) +
  scale_fill_gradient(low = "#f7fbff", high = "#3d97d1", labels = scales::comma) +
  labs(
    title = "Matrice des surfaces parcellaires (m²) - Saint-Émilion",
    subtitle = "Croisement Parcelles candidates (TRUE/FALSE) × Présence LiDAR",
    x = "Présence LiDAR",
    y = "Parcelle candidate (TRUE / FALSE)",
    fill = "Surface (m²)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# --- 5.2 Matrice des surfaces (CVI vs LiDAR) ---
tableau_surfaces_CVI <- xtabs(surface_parcelle ~ presence_CVI + presence_lidar, data = df_visu)
df_surface_CVI <- as.data.frame(tableau_surfaces_CVI)

df_surface_CVI$presence_CVI <- factor(df_surface_CVI$presence_CVI, levels = c("FALSE", "TRUE"))
df_surface_CVI$presence_lidar <- factor(df_surface_CVI$presence_lidar, levels = c("TRUE", "FALSE"))

p_surface_CVI <- ggplot(df_surface_CVI, aes(x = presence_lidar, y = presence_CVI, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = format(round(Freq), big.mark = " ")),
            color = "black", fontface = "bold", size = 4.5) +
  scale_fill_gradient(low = "#f7fcf5", high = "#30c761", labels = scales::comma) +
  labs(
    title = "Matrice des surfaces parcellaires (m²) - Saint-Émilion",
    subtitle = "Croisement Parcelles CVI (TRUE/FALSE) × Présence LiDAR",
    x = "Présence LiDAR",
    y = "Présence CVI",
    fill = "Surface (m²)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# --- 5.3 Matrice de validation (Candidat vs CVI) ---
tableau_surfaces_val <- xtabs(surface_parcelle ~ presence_CVI + candidate_vigne, data = df_visu)
df_surface_val <- as.data.frame(tableau_surfaces_val)

df_surface_val$presence_CVI <- factor(df_surface_val$presence_CVI, levels = c("FALSE", "TRUE"))
df_surface_val$candidate_vigne <- factor(df_surface_val$candidate_vigne, levels = c("TRUE", "FALSE"))

p_surface_val <- ggplot(df_surface_val, aes(x = candidate_vigne, y = presence_CVI, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = format(round(Freq), big.mark = " ")),
            color = "black", fontface = "bold", size = 4.5) +
  scale_fill_gradient(low = "#fcfbfd", high = "#cd81f0", labels = scales::comma) +
  labs(
    title = "Matrice des surfaces parcellaires (m²) - Saint-Émilion",
    subtitle = "Croisement Parcelles candidates × Présence CVI",
    x = "Parcelle candidate (TRUE / FALSE)",
    y = "Présence CVI (TRUE / FALSE)",
    fill = "Surface (m²)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# --- 5.4 Histogramme superposé ---
p_hist_superpose <- ggplot(df_visu, aes(x = pct_veg, fill = candidate_vigne)) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity", color = "white") +
  scale_fill_manual(
    values = c("TRUE" = "#238b45", "FALSE" = "#cb181d"),
    labels = c("TRUE" = "Parcelles candidates (OSM/CoSIA > 5%)", "FALSE" = "Parcelles non candidates (OSM/CoSIA < 5%)")
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(
    title = "Distribution de la végétation LiDAR - Saint-Émilion",
    x = "Proportion de végétation LiDAR (%)",
    y = "Nombre de parcelles",
    fill = "Statut"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# --- 5.5 Histogramme superposé (CVI) ---
p_hist_superpose_CVI <- ggplot(df_visu, aes(x = pct_veg, fill = presence_CVI)) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity", color = "white") +
  scale_fill_manual(
    values = c("TRUE" = "#2171b5", "FALSE" = "#cb181d"),
    labels = c("TRUE" = "Présentes dans le CVI", "FALSE" = "Absentes du CVI")
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(
    title = "Distribution de la végétation LiDAR selon le CVI - Saint-Émilion",
    x = "Proportion de végétation LiDAR (%)",
    y = "Nombre de parcelles",
    fill = "Statut CVI"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))

# =========================================================================
# 6. AFFICHAGE DES RÉSULTATS
# =========================================================================
print(p_surface)
print(p_surface_CVI)
print(p_surface_val)
print(p_hist_superpose)
print(p_hist_superpose_CVI)