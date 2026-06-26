<table width="100%">
  <tr>
    <td align="left">
      <img src="img/CESAER.jpg" alt="CESAER" width="120">
    </td>
    <td align="right">
      <img src="img/INRAE.png" alt="INRAE" width="120" style="margin-right: 15px;">
      <img src="img/AGRO.png" alt="Institut Agro Dijon" width="180">
    </td>
  </tr>
</table>

# Analyse de la variabilité structurelle du couvert viticole par LiDAR HD

Ce dépôt contient le code source développé dans le cadre du mémoire de Master 2 "Observation de la Terre et Géomatique" (Université de Strasbourg). L'étude porte sur la caractérisation de l'architecture du couvert végétal dans le vignoble girondin à l'aide de données LiDAR Haute Densité IGN, et sur l'analyse de ses déterminants environnementaux et anthropiques.

## Contexte et Objectifs

L'objectif de ce projet est de modéliser la variabilité structurelle des parcelles viticoles en croisant des métriques dérivées du LiDAR avec des variables topographiques et agronomiques. La méthodologie repose sur une chaîne de traitement géomatique complète, de la donnée brute à la modélisation statistique multivariée.

Les objectifs spécifiques du code incluent :
* Le prétraitement des nuages de points LiDAR (génération de MNT locaux par interpolation IDW k-NN et calcul de Modèles de Hauteur de Canopée - CHM).
* L'extraction de métriques structurelles à l'échelle de la parcelle (hauteur moyenne, hétérogénéité, couverture spatiale normalisée).
* L'extraction de variables topographiques par statistiques zonales (Altitude, Pente, Exposition, TWI) à partir du RGE ALTI (5m).
* L'intégration et le filtrage des données déclaratives agronomiques (Casier Viticole Informatisé - CVI).
* L'analyse statistique des profils morphologiques (ANOVA, corrélations de Pearson, Analyse en Composantes Principales - ACP, et Classification Ascendante Hiérarchique - CAH).

## Données mobilisées

Le pipeline de traitement a été conçu pour exploiter les sources de données suivantes :
* **Parcellaire Express et ADMIN EXPRESS**
* **LiDAR HD** : Institut National de l'Information Géographique et Forestière (IGN).
* **Topographie** : RGE ALTI® 5m (IGN).
* **Agronomie** : Casier Viticole Informatisé (CVI).
* **Occupation du sol** : OpenStreetMap (OSM) et CoSIA.

## Architecture du projet

Le dépôt est structuré autour des grandes étapes du traitement méthodologique :

## Contexte du projet
* **Auteur :** Théo Liegeon
* **Structure d'accueil :** CESAER (UMR1041 INRAE - Institut Agro Dijon)
* **Partenaire académique :** Laboratoire Image, Ville, Environnement (UMR7362 CNRS-Unistra)
* **Année :** 2025 - 2026

*Ce code est fourni à des fins d'évaluation académique et de documentation méthodologique.*

<div style="position:relative; width:100%; margin-top:50px; padding:20px 0; text-align:center;">
  <img src="img/OTG.png" alt="OTG" width="120">
  <img src="img/FAC.png"
       alt="Faculté de Géographie"
       width="180"
       style="position:absolute; right:20px; top:50%; transform:translateY(-50%);">
</div>
