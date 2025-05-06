if (!require("sf")) install.packages("sf")
if (!require("dbscan")) install.packages("dbscan")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("leaflet")) install.packages("leaflet")

library(sf)
library(dbscan)
library(ggplot2)
library(dplyr)

setwd("~/Desktop/Teluq/SCI-1402/Data/")
addComplet <- read.csv("001.csv", sep=";", header = TRUE, encoding="UTF-8")
addSansIHV <- read.csv("003.csv", sep=";", header = TRUE, encoding="UTF-8")

print(head(addComplet))

# Effectuer une jointure interne sur la colonne adresse_id
resultat <- inner_join(addComplet, addSansIHV, by = "adresse_id")

# Convertir les colonnes Latitude et Longitude en numériques
# Nettoyer les champs et remplacer les virgules par des points
resultat$Latitude <- as.numeric(gsub(",", ".", resultat$Latitude))
resultat$Longitude <- as.numeric(gsub(",", ".", resultat$Longitude))

# Remplacer les valeurs "NULL" par "" dans tout le data frame
resultat[resultat == "NULL"] <- ""

# Afficher le résultat
print(head(resultat))

# Échantillon aléatoire de 40 000 lignes (ou tout le fichier si moins de 40k)
n_max <- min(40000, nrow(addComplet))
set.seed(123)
n_max <- min(40000, nrow(addComplet))
addComplet_leger <- addComplet %>%
  slice_sample(n = n_max)

# Sortie d'un fichier plus léger pour le dépôt sur GitHub
# S'assurer que les colonnes sont identiques
names(resultat) == names(addComplet_leger)  # doit retourner TRUE


addComplet_leger$Latitude <- as.numeric(gsub(",", ".", addComplet_leger$Latitude))
addComplet_leger$Longitude <- as.numeric(gsub(",", ".", addComplet_leger$Longitude))

# Fusion verticale des deux data frames
donnees_combinees <- bind_rows(addComplet_leger, resultat)

# Exporter, une version plus légère (83K lignes) de 001.csv sera déposée sur Github, la version complète (3,5M lignes) est sur OneDrive de Teluq
write_csv(donnees_combinees, "001-combine.csv")
# Info: le fichier 001-combine.csv sera déposé sur GitHub avec le nom 001.csv

#Fichier de sortie qui servira aux travaux des regroupements des points, second livrable
# Fichier pour la suite du projet
write_csv(resultat, "resultat_sf.csv")


#--------------------------------------------------------------------
# Statistiques du nombre de points par régions administrative

library(dplyr)
library(tidyr)

# Renommer les colonnes avant la fusion pour éviter les conflits
points_cluster <- resultat_sf %>%
  st_drop_geometry() %>%
  count(Region_ADM, name = "Nombre_de_points_cluster")

points_total <- addComplet %>%
  count(Region_ADM, name = "Nombre_de_points_total")

# 3. Fusionner les deux tableaux
comparatif_points <- full_join(points_cluster, points_total, by = "Region_ADM") %>%
  # 4. Remplacer les NA par 0
  mutate(
    Nombre_de_points_cluster = replace_na(Nombre_de_points_cluster, 0),
    Nombre_de_points_total = replace_na(Nombre_de_points_total, 0)
  ) %>%
  # 5. Ajouter le pourcentage
  mutate(
    Pourcentage_non_couvert = ifelse(Nombre_de_points_total == 0, "0%",
                                 paste0(round(100 * Nombre_de_points_cluster / Nombre_de_points_total, 1), "%"))

  ) %>%
  arrange(desc(Pourcentage_non_couvert))

# Remplacer les NA par 0 si certaines régions sont absentes dans l’un des deux jeux
comparatif_points[is.na(comparatif_points)] <- 0

# Afficher le résultat
print(comparatif_points)

# Fichier de sortie qui servira pour la suite du projet
write_csv(comparatif_points, "comparatif_points.csv")



#----------------------------------------------------------------------------
# Prototype de carte: les vraies cartes seront dans le quatrième livrables
# Visualiser rapidement les résultats des adresses

library(leaflet)

# Créer un objet spatial sf à partir des coordonnées
resultat_sf <- st_as_sf(resultat, 
                        coords = c("Longitude", "Latitude"), 
                        crs = 4326, 
                        remove = FALSE)  # Garde les colonnes originales

resultat_sf$point_coleur <- ifelse(resultat_sf$Region_ADM %in% c("Montréal", "Capitale-Nationale", "Laval"),
                                   "red",  # Couleur rouge pour les régions spécifiées
                                   "blue")  # Couleur bleue pour les autres régions

# Carte - Leaflet
leaflet(resultat_sf) %>%
  addTiles(
    urlTemplate = "https://geoegl.msp.gouv.qc.ca/carto/tms/1.0.0/carte_gouv_qc_public@EPSG_3857/{z}/{x}/{-y}.png",
    attribution = "© Gouvernement du Québec"
  ) %>%
  # Couche WMS : contours des polygones
  addWMSTiles(
    baseUrl = "https://servicescarto.mern.gouv.qc.ca/pes/services/Territoire/SDA_WMS/MapServer/WMSServer",
    layers = "Région administrative",  # ou ajuste selon la couche voulue
    options = WMSTileOptions(
      format = "image/png",
      transparent = TRUE
    ),
    attribution = "© MERN - Gouvernement du Québec",
    group = "Contours SDA"
  ) %>%
  addCircleMarkers(
    radius = 3,
    color = ~point_coleur, 
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste(
      "Adresse ID :", adresse_id, "<br>",
      "Region Administrative :", Region_ADM, "<br>",
      "Municipalité :", munnom, "<br>",
      "Adresse :", No_civique,  No_civique_suffix, " ", Nom_rue
    )
  )

