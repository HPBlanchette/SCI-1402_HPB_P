# Installer les librairies si nécessaires
if (!require("leaflet")) install.packages("leaflet")
if (!require("sf")) install.packages("sf")
if (!require("dplyr")) install.packages("dplyr")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("plotly")) install.packages("plotly")
if (!require("geojsonio")) install.packages("geojsonio")
if (!require("leaflet")) install.packages("leaflet")
if (!require("dbscan")) install.packages("dbscan")
if (!require("sf")) install.packages("sf")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggforce")) install.packages("ggforce")
if (!require("kableExtra")) install.packages("kableExtra")


# État de la sitiuation
library(readr)
library(dplyr)
library(leaflet)
library(sf)
library(RColorBrewer)

# Info sur les paramètres de recherche
eps_values <- seq(0.01, 0.06, by = 0.01)

# Approx. des distances en m pour une lat de 46°N
lat_m <- 111320  # mètre pour 1° de latitude
lon_m <- 111320 * cos(46 * pi / 180)  # mètre pour 1° de longitude à 46°N

# Création du df
df_eps_m <- data.frame(
  Eps_degre = eps_values,
  Distance_NS_m = round(eps_values * lat_m, 0),
  Distance_EW_m = round(eps_values * lon_m, 0)
)

colnames(df_eps_m) <- c("Eps (°)", "Distance nord-sud (m)", "Distance est-ouest (m)")

library(knitr)
kable(df_eps_m, caption = "Correspondance entre eps (en degrés) et les distances approximatives en mètres à 46°N")

# Lire les données de base
setwd("~/Desktop/Teluq/SCI-1402/Data/")
df <- read.csv("resultat_sf.csv", sep = ",", header = TRUE, encoding = "UTF-8")

# Nettoyer les données
df_clean <- df %>%
  filter(!is.na(Longitude), !is.na(Latitude), !is.na(munnom)) %>%
  mutate(
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude),
    Region_ADM = as.factor(Region_ADM)
  )

# Générer une palette de couleurs selon Region_ADM
Region_ADM_levels <- levels(df_clean$Region_ADM)
palette_Region_ADM_levels <- colorFactor(palette = brewer.pal(min(length(Region_ADM_levels), 8), "Set1"), domain = Region_ADM_levels)

carte_reg_adm <- leaflet(df_clean) %>%
  addTiles(group = "Base") %>%
  addWMSTiles(
    baseUrl = "https://servicescarto.mern.gouv.qc.ca/pes/services/Territoire/SDA_WMS/MapServer/WMSServer",
    layers = "Région administrative",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "© MERN - Gouvernement du Québec",
    group = "Contours SDA"
  ) %>%
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    radius = 4,
    color = ~palette_Region_ADM_levels(Region_ADM),
    fillOpacity = 0.8,
    stroke = FALSE,
    label = ~paste0("Municipalité: ", munnom, "<br>Latitude: ", Latitude, "<br>Longitude: ", Longitude)
  ) %>%
  addLegend(
    position = "bottomright",
    pal = palette_Region_ADM_levels,
    values = ~Region_ADM,
    title = "Region_ADM",
    opacity = 1)

carte_reg_adm


#Stat par région

stats_region <- df_clean %>%
  filter(!is.na(Region_ADM)) %>%
  group_by(Region_ADM) %>%
  summarise(
    Nombre_points = n()
  ) %>%
  arrange(desc(Nombre_points))

library(kableExtra)

stats_region %>%
  kable("html", caption = "Nombre de points par région administrative") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))


# Autre carte par région

# Lire les centroïdes des régions
df_centroïdes <- read_csv("centroides_regions_quebec.csv")

# Lire le fichier des pourcentages non couverts
df_points <- read_csv("comparatif_points.csv")

# Nettoyer la colonne "pourcentage_non_couvert"
df_points <- df_points %>%
  mutate(
    pourcentage_non_couvert = as.numeric(gsub("%", "", Pourcentage_non_couvert))
  )

# Fusionner sur le code de région
df_merged <- df_centroïdes %>%
  left_join(df_points, by = c("Nom" = "Region_ADM"))

# Créer un objet sf à partir des centroïdes
sf_regions <- st_as_sf(df_merged, coords = c("Longitude", "Latitude"), crs = 4326)

# Créer une palette de couleurs rouge → vert (valeur élevée = rouge)
pal <- colorNumeric(
  palette = "RdYlGn",
  domain = sf_regions$pourcentage_non_couvert,
  reverse = TRUE
)

leaflet(sf_regions) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    radius = ~sqrt(pourcentage_non_couvert) * 10,
    color = ~pal(pourcentage_non_couvert),
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste0(
      "<strong>", Nom, "</strong><br>",
      "Pourcentage non couvert : ", round(pourcentage_non_couvert, 1), "%"
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~pourcentage_non_couvert,
    title = "Pourcentage des adresses non couvertes (%)",
    opacity = 0.8
  )



library(readr)
library(dplyr)
library(dbscan)
library(leaflet)
library(sf)
library(geojsonio)
library(RColorBrewer)


coords <- df_clean %>%
  select(Longitude, Latitude) %>%
  as.matrix()

# Paramètres DBSCAN
eps_values <- seq(0.01, 0.03, by = 0.01)
minPts_values <- seq(70, 30, by = -10)

# Créer la carte Leaflet
carte <- leaflet() %>%
  addTiles(group = "Base") %>%
  addWMSTiles(
    baseUrl = "https://servicescarto.mern.gouv.qc.ca/pes/services/Territoire/SDA_WMS/MapServer/WMSServer",
    layers = "Région administrative",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "© MERN - Gouvernement du Québec",
    group = "Contours SDA"
  )

# Liste pour garder le nom des groupes
group_names <- c()

# Générer les itérations de clustering
for (eps in eps_values) {
  for (minPts in minPts_values) {
    label <- sprintf("eps=%.2f_minPts=%d", eps, minPts) 
    group_names <- c(group_names, label)
    
    # Appliquer DBSCAN
    db <- dbscan(coords, eps = eps, minPts = minPts)
    df_iter <- df_clean %>%
      mutate(cluster = as.factor(db$cluster))
    
    df_iter_grouped <- df_iter %>%
      filter(cluster != 0)
    
    # Ajouter le nombre de points par cluster
    cluster_counts <- df_iter_grouped %>%
      count(cluster, name = "n_points")
    
    df_iter_grouped <- df_iter_grouped %>%
      left_join(cluster_counts, by = "cluster")
    
    # Ajouter à la carte dans un groupe spécifique
    carte <- carte %>%
      addCircleMarkers(
        data = df_iter_grouped,
        lng = ~Longitude,
        lat = ~Latitude,
        #color = ~ifelse(cluster == 0, "gray", RColorBrewer::brewer.pal(8, "Set1")[as.integer(cluster)]),
        color = ~palette_Region_ADM_levels(Region_ADM),
        radius = ~ifelse(cluster == 0, 1, 4),
        fillOpacity = 0.8,
        stroke = FALSE,
        group = label,
        label = ~paste("Cluster:", cluster, ": Nb points:", n_points)
      )
  }
}


# Ajouter le contrôle des couches avec baseGroups
carte <- carte %>%
  addLayersControl(
    baseGroups = group_names,
    overlayGroups = c("Contours SDA"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(setdiff(group_names, group_names[1]))  # Cacher tous sauf le 1er

carte

# Statistiques par itération DBSCAN pour le province au complet
res_dbscan <- list()

# Refaire les itérations pour calculer les stats
for (eps in eps_values) {
  for (minPts in minPts_values) {
    label <- sprintf("eps=%.2f_minPts=%d", eps, minPts)
    
    # Appliquer DBSCAN
    db <- dbscan(coords, eps = eps, minPts = minPts)
    clusters <- db$cluster
    
    n_total <- length(clusters)
    n_bruit <- sum(clusters == 0)
    n_regroupes <- n_total - n_bruit
    n_clusters <- length(unique(clusters)) - ifelse(any(clusters == 0), 1, 0) 
    
    pourcentage_regroupes <- (n_regroupes / n_total) * 100
    
    res_dbscan[[label]] <- data.frame(
      Paramètres = label,
      Nombre_de_clusters = n_clusters,
      Nombre_de_points_regroupes = n_regroupes,
      Pourcentage_de_points_regroupes = round(pourcentage_regroupes, 1)
    )
  }
}

# Combiner tous les résultats en un seul tableau
df_stats_dbscan <- do.call(rbind, res_dbscan)

library(knitr)
library(kableExtra)

# Afficher le tableau avec bordure
kable(df_stats_dbscan, caption = "Résumé statistique pour chaque itération de DBSCAN", format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "bordered"), full_width = FALSE)

# Statistiques par itération DBSCAN par région administrative

# Extraire les minPts et eps depuis les labels
df_stats_dbscan <- df_stats_dbscan %>%
  mutate(
    eps = as.numeric(sub(".*eps=([0-9.]+)_minPts=.*", "\\1", rownames(df_stats_dbscan))),
    minPts = as.numeric(sub(".*minPts=([0-9]+)", "\\1", rownames(df_stats_dbscan)))
  )

library(ggplot2)

# Nuage de points : taille selon Nombre de clusters, couleur selon % de points regroupés
ggplot(df_stats_dbscan, aes(x = minPts, y = eps)) +
  geom_point(aes(size = Nombre_de_clusters, color = Pourcentage_de_points_regroupes), alpha = 0.8) +
  geom_text(aes(label = paste0(Pourcentage_de_points_regroupes, "%")), vjust = -0.5, size = 3) +
  scale_color_gradient(low = "red", high = "green") +
  scale_size_continuous(range = c(3, 10)) +  # contrôle la plage de taille
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  scale_x_reverse() +
  labs(
    title = "Nuage de points: minPts vs eps",
    subtitle = "Taille = Nombre de clusters, Couleur = % de points regroupés",
    x = "Nombre minimum de points (minPts)",
    y = "Distance maximum (eps)",
    color = "% de points regroupés",
    size = "Nombre de clusters"
  ) +
  theme_minimal()

# DBSCAN par région
regions <- unique(df_clean$Region_ADM)

res_dbscan_region <- list()

for (region in regions) {
  
  df_region <- df_clean %>% filter(Region_ADM == region)
  
  coords_region <- df_region %>% 
    select(Longitude, Latitude) %>% 
    as.matrix()
  
  for (eps in eps_values) {
    for (minPts in minPts_values) {
      label <- sprintf("eps=%.2f_minPts=%d", eps, minPts)
      
      # Appliquer DBSCAN sur la région
      db <- dbscan(coords_region, eps = eps, minPts = minPts)
      clusters <- db$cluster
      
      n_total <- length(clusters)
      n_bruit <- sum(clusters == 0)
      n_regroupes <- n_total - n_bruit
      n_clusters <- length(unique(clusters)) - ifelse(any(clusters == 0), 1, 0)
      
      pourcentage_regroupes <- (n_regroupes / n_total) * 100
      
      res_dbscan_region[[paste(region, label)]] <- data.frame(
        Region_ADM = region,
        eps = eps,
        minPts = minPts,
        Nombre_de_clusters = n_clusters,
        Nombre_de_points_regroupes = n_regroupes,
        Pourcentage_de_points_regroupes = round(pourcentage_regroupes, 2)
      )
    }
  }
}



library(ggforce)

# Combiner tous les résultats
df_stats_dbscan_region <- do.call(rbind, res_dbscan_region)

# Déterminer le nombre total de régions
n_regions <- length(unique(df_stats_dbscan_region$Region_ADM))

# Déterminer le nombre de pages (2 régions par page)
n_pages <- ceiling(n_regions / 2)

for (i in 1:n_pages) {
  p <- ggplot(df_stats_dbscan_region, aes(x = minPts, y = eps)) +
    geom_point(aes(size = Nombre_de_clusters, color = Pourcentage_de_points_regroupes), alpha = 0.8) +
    geom_text(aes(label = paste0(Pourcentage_de_points_regroupes, "%")), vjust = -0.5, size = 2.5) +
    scale_color_gradient(low = "red", high = "green") +
    scale_size_continuous(range = c(2, 8)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    scale_x_reverse() +
    ggforce::facet_wrap_paginate(
      ~Region_ADM,
      ncol = 1,
      nrow = 2,   
      page = i
    ) +
    labs(
      title = "Clustering DBSCAN par Région administrative",
      subtitle = paste("Page", i, "sur", n_pages),
      x = "Nombre minimum de points (minPts)",
      y = "Distance maximum (eps)",
      color = "% de points regroupés",
      size = "Nombre de clusters"
    ) +
    theme_minimal()
  
  print(p)
}


## Évalutation de la performance du modèle DBSCAN

# Tester la performance du calcul DBSCAN
library(tibble)  # pour tibble::tibble si besoin

# Créer une liste pour stocker les temps
perf_dbscan <- list()


eps_values <- seq(0.001, 0.05, by = 0.01)         # 0.01 à 0.10 (10 valeurs)
minPts_values <- seq(5, 50, by = 5)          # 10 à 100 (10 valeurs)

# Mesurer les temps pour chaque combinaison de paramètres
for (eps in eps_values) {
  for (minPts in minPts_values) {
    start_time <- Sys.time()
    
    db <- dbscan(coords, eps = eps, minPts = minPts)
    
    end_time <- Sys.time()
    elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    label <- sprintf("eps=%.2f_minPts=%d", eps, minPts)
    
    perf_dbscan[[label]] <- tibble(
      Paramètres = label,
      eps = eps,
      minPts = minPts,
      Temps_en_secondes = round(elapsed, 4)
    )
  }
}

# Combiner tous les résultats dans un data frame
df_perf_dbscan <- do.call(rbind, perf_dbscan)

# Afficher le tableau des temps de calcul
knitr::kable(df_perf_dbscan, caption = "Temps de calcul pour chaque itération de DBSCAN (en secondes)")


ggplot(df_perf_dbscan, aes(x = minPts, y = eps)) +
  geom_point(aes(size = Temps_en_secondes, color = Temps_en_secondes), alpha = 0.8) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_size_continuous(range = c(2, 10)) +
  labs(
    title = "Temps de calcul de DBSCAN",
    subtitle = "Taille et couleur = temps de calcul (en secondes)",
    x = "minPts",
    y = "eps",
    size = "Temps (s)",
    color = "Temps (s)"
  ) +
  theme_minimal()



library(plotly)

# Graphique 3D interactif des temps de calcul DBSCAN
plot_ly(
  df_perf_dbscan,
  x = ~minPts,
  y = ~eps,
  z = ~Temps_en_secondes,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 4,
    color = ~Temps_en_secondes,
    colorscale = "Viridis",
    showscale = TRUE
  )
) %>%
  layout(
    title = "Temps de calcul DBSCAN (3D), en Fc des parmams",
    scene = list(
      xaxis = list(title = "minPts"),
      yaxis = list(title = "eps"),
      zaxis = list(title = "Temps (secondes)")
    )
  )


