
# ---------------------- ETUDE DES FEATURES ET NOTES ----------------------

# Base = véhicule
# DateTime = date et heure du début du trajet
# Lat et Lon = coords du début du trajet
#les coords correspondent a la ville de New York

# ---------------------- INSTALLATION LIBRAIRIES ----------------------

install.packages('dplyr')
install.packages('ggplot2')
install.packages('lubridate')
install.packages('scales')
install.packages('tidyr')
install.packages("leaflet")
install.packages("geosphere")
install.packages("stats")
install.packages("Metrics")


# ---------------------- CHARGER LES LIBRAIRIES ----------------------

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)
library(reshape2)
library(leaflet)
library(geosphere)
library(stats)
#library(leaflet.extras)
library(zoo)
library(Metrics)
library(caret)


Sys.setlocale("LC_TIME", "en_US.UTF-8")


# ---------------------- DATA COLLECTION ----------------------

#BOLT DATA
file_path <- "~/BoltDatasets"
files <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE)

dt <- do.call(rbind, lapply(files, read.csv, stringsAsFactors = FALSE))

#creating key to be able to join the datasets
dt$Date.Time <- as.POSIXct(dt$Date.Time, format="%m/%d/%Y %H:%M:%S")
dt$date <- as.Date(dt$Date.Time)


# WEATHER DATA

# Charger et convertir les données météo de 2016
file_path_weather_16 <- "~/archive_16/weather_data_nyc_centralpark_2016.csv"
dt_weather_16 <- read.csv(file_path_weather_16, check.names = FALSE)

# Convertir la colonne date (format "jour-mois-année") et changer l'année pour 2014
dt_weather_16$date <- as.Date(dt_weather_16$date, format="%d-%m-%Y")
dt_weather_16$date <- as.Date(format(dt_weather_16$date, "2014-%m-%d"))

# Charger et convertir les données météo de 2019
file_path_weather_19 <- "~/archive_19/nyc_temperature.csv"
dt_weather_19 <- read.csv(file_path_weather_19, check.names = FALSE)
head(dt_weather_19)

# Convertir la colonne date (format "jour/mois/année") et changer l'année pour 2014
dt_weather_19$date <- as.Date(dt_weather_19$date, format="%d/%m/%y")  # %d pour jour, %m pour mois, %y pour année à deux chiffres
dt_weather_19$date <- as.Date(format(dt_weather_19$date, "2014-%m-%d"))

# Fonction pour harmoniser les données météo
prepare_weather_data <- function(data, year) {
  if (year == 2016) {
    data$date <- as.Date(data$date, format = "%d-%m-%Y")
    data$tmax <- data$`maximum temperature`
    data$tmin <- data$`minimum temperature`
    data$tavg <- data$`average temperature`
    data$precipitation <- as.numeric(data$precipitation)
  } else {
    data$date <- as.Date(data$date, format = "%d/%m/%y")
    data$precipitation <- as.numeric(ifelse(data$precipitation == "T", 0, data$precipitation))
  }
  
  data$date <- as.Date(format(data$date, "2014-%m-%d"))
  data$year <- year
  
  return(data %>% select(date, tmax, tmin, tavg, precipitation, year))
}

dt_weather_16 <- prepare_weather_data(dt_weather_16, 2016)
dt_weather_19 <- prepare_weather_data(dt_weather_19, 2019)

# Fusion des données météo
weather_data <- rbind(dt_weather_16, dt_weather_19)

# Calcul des moyennes météo
avg_weather <- weather_data %>%
  group_by(date) %>%
  summarise(
    tmax = mean(tmax, na.rm = TRUE),
    tmin = mean(tmin, na.rm = TRUE),
    tavg = mean(tavg, na.rm = TRUE),
    precipitation = mean(precipitation, na.rm = TRUE),
    .groups = 'drop'
  )

# Vérification des données harmonisées
print(head(avg_weather))

# Fusion des données Bolt et météo
dt <- dt %>%
  left_join(avg_weather, by = "date")

head(dt)
summary(dt)



# ----------------------------------- DATA CLEANING ----------------------------------------

nb_total_na <- sum(is.na(dt))
print(paste("Le nombre total de valeur nulles est de :", nb_total_na))


# nb_duplicates <- sum(duplicated(dt))
# print(paste("Le nombre total de valeur en double est de :", nb_duplicates))


#vérification des coords et des base
valid_lat <- sum(dt$Lat >= -90 & dt$Lat <= 90)
valid_long <- sum(dt$Lon >= -180 & dt$Lon <= 180)
valid_base <- grepl("^B0", dt$Base) & nchar(dt$Base) == 6

if(all(valid_base)) {
  print("Toutes les valeurs de la colonne 'Base' commencent par 'B0' et ont une longueur de 6 caractères.")
} else {
  invalid_values <- dt[!valid_base, ]
  print("Valeurs incorrectes :")
  print(invalid_values)
}


#------------------------AFFICHAGE DES COORD MAX POUR AVOIR UN IDEE VISUELLE-------------------------

# Créer une carte interactive centrée sur New York avec un fond OpenStreetMap
m <- leaflet() %>%
  addTiles() %>% 
  setView(lng = -73.935242, lat = 40.730610, zoom = 12) 

# Trouver les points extrêmes (nord, sud, est, ouest)
point_north <- dt[which.max(dt$Lat), ]
point_south <- dt[which.min(dt$Lat), ]
point_east <- dt[which.max(dt$Lon), ]
point_west <- dt[which.min(dt$Lon), ]

# Ajouter ces points extrêmes dans un data frame
extreme_coordinates <- data.frame(
  lon = c(point_north$Lon, point_south$Lon, point_east$Lon, point_west$Lon),
  lat = c(point_north$Lat, point_south$Lat, point_east$Lat, point_west$Lat),
  labels = c("North", "South", "East", "West")
)

# Ajouter des marqueurs pour les points extrêmes
m <- m %>%
  addMarkers(lng = extreme_coordinates$lon, lat = extreme_coordinates$lat, popup = extreme_coordinates$labels)

# Afficher la carte interactive
m

# Affiche que 3 points car le point le plus au nord est aussi le point le pls a l'est
# On constate que les points sont très éloigné, il faut qu'on utilise le Z-score pour
#filtrer les données aberantes qui sont trop loins du centre de NY

# ---------------------- CALCUL Z-SCORE POUR SUPPRIMER LES VALEURS ABERRANTES ----------------------

# # Fonction pour calculer le Z-score
# calculate_zscore <- function(x) {
#   (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
# }
# 
# # Calcul des Z-scores pour la latitude et la longitude
# dt$lat_zscore <- calculate_zscore(dt$Lat)
# dt$lon_zscore <- calculate_zscore(dt$Lon)
# 
# # Filtrer les points avec un Z-score supérieur à 3 (aberrants)
# outliers <- dt[abs(dt$lat_zscore) > 3 | abs(dt$lon_zscore) > 3, ]
# 
# # Afficher le nombre de valeurs aberrantes
# print(paste("Nombre de valeurs aberrantes détectées :", nrow(outliers)))

#--------------------------DELIMITATION AVEC COORDS----------------------------------------


# Définir les deux points formant les coins du carré
point1 <- c(lat = 40.618607, lon = -74.240206)
point2 <- c(lat = 40.809485, lon = -73.744362)

# Déterminer les limites du carré
lat_min <- min(point1['lat'], point2['lat'])
lat_max <- max(point1['lat'], point2['lat'])
lon_min <- min(point1['lon'], point2['lon'])
lon_max <- max(point1['lon'], point2['lon'])

# Filtrer les points en dehors du carré
outliers <- dt[dt$Lat < lat_min | dt$Lat > lat_max | dt$Lon < lon_min | dt$Lon > lon_max, ]

# Afficher le nombre de points supprimés
print(paste("Nombre de points supprimés :", nrow(outliers)))

# Filtrer les points à l'intérieur du carré (ceux qu'on souhaite garder)
dt <- dt[dt$Lat >= lat_min & dt$Lat <= lat_max & dt$Lon >= lon_min & dt$Lon <= lon_max, ]

# Sélectionner 2% des points supprimés pour l'affichage
sample_size <- ceiling(0.02 * nrow(outliers)) # 2% des valeurs aberrantes
outliers_sample <- outliers[sample(nrow(outliers), sample_size), ] # Échantillon aléatoire


# ---------------------- AFFICHAGE DES VALEURS SUPPRIMÉES SUR LA CARTE ----------------------

# Créer une carte interactive centrée sur la zone des points supprimés
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = mean(outliers_sample$Lon), lat = mean(outliers_sample$Lat), zoom = 12)

# Ajouter les marqueurs pour les points supprimés
if (nrow(outliers_sample) > 0) {
  popups <- paste("Lat:", outliers_sample$Lat, "<br>Lon:", outliers_sample$Lon)
  
  # Ajouter des marqueurs rouges pour les points supprimés
  m <- m %>%
    addCircleMarkers(lng = outliers_sample$Lon, lat = outliers_sample$Lat,
                     color = "red", radius = 5, popup = popups)
} else {
  print("Aucun point supprimé à afficher.")
}

# Afficher la carte interactive
m

# ----------------------Extraire  mois, jour, heure ----------------------


dt$Month <- factor(month(dt$Date.Time, label = TRUE, abbr = FALSE))
dt$day <- wday(dt$Date.Time, label = TRUE, abbr = FALSE)
dt$hour <- hour(dt$Date.Time)

dt$day_of_month <- day(dt$Date.Time)
dt$date <- as.Date(dt$Date.Time)


summary(dt)

# -----------------------------Basic Data Analysis ----------------------------

dt$date <- as.Date(dt$Date.Time)
rides_per_day <- summarise(group_by(dt, date), rides = n())
rides_per_month <- summarise(group_by(dt, Month), rides = n())

total_rides <- nrow(dt)
mean_ride_per_day <- mean(rides_per_day$rides, na.rm = TRUE)
mean_ride_per_month <- mean(rides_per_month$rides)

print(paste("Le nombre total de trajets est de", total_rides))
print(paste("Le nombre moyen de trajets par jour est de", round(mean_ride_per_day, 0)))
print(paste("Le nombre moyen de trajets par mois est de", round(mean_ride_per_month, 0)))

# Identifier le jour avec le plus grand nombre de trajets
peak_day <- rides_per_day[which.max(rides_per_day$rides), ]
print(paste("Le jour du mois avec le plus grand nombre de trajets est le", peak_day$date))
print(paste("Le nombre de trajets ce jour-là est de", peak_day$rides))

#Nombre de base : 
nombre_bases_uniques <- n_distinct(dt$Base)
cat("Nombre de bases différentes :", nombre_bases_uniques, "\n")


# -----------------------------Etude trajets par mois ----------------------------

# Compter le nombre de trajets par mois et les afficher sous forme de graphique en barres
ggplot(data = as.data.frame(table(dt$Month)), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Months") +
  ylab("Number of trips") +
  ggtitle("Number of trips per month in NY")

# ----------------------------- Etude trajets par semaine  ----------------------------

rides_per_day_of_week <- group_by(dt, day)
rides_per_day_of_week <- summarise(rides_per_day_of_week, rides = n(), .groups = 'drop')

ggplot(data = rides_per_day_of_week, aes(x = day, y = rides)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Day of the Week") +
  ylab("Number of trips") +
  ggtitle("number of trips per Week") +
  scale_y_continuous(labels = comma) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))


# ----------------------------- Etude trajets par heure  ----------------------------


# Créer un résumé des trajets par jour de la semaine et par heure
summary <- aggregate(list(count = dt$Date.Time), 
                     by = list(day = dt$day, hour = dt$hour), 
                     FUN = length)
head(summary)


# Créer un graphique à barres à partir du résumé
ggplot(data = summary, aes(x = day, y = count, fill = factor(hour))) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Hour of the Day") +
  ylab("Number of Trips") +
  ggtitle("Number of Bolt trips per day of the week and per hour") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = "Hour")) +
  theme(legend.position = "bottom") +
  scale_x_discrete()


## ----------------- tableau croisé entre les jours et les heures  ----------------------------


ride_per_hour_in_day <- table(dt$hour, dt$day)
ride_per_hour_in_day_df <- as.data.frame(ride_per_hour_in_day)

# Créer une heatmap pour visualiser les données avec des couleurs dégradées
ggplot(ride_per_hour_in_day_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Hour", y = "Day", fill = "Frequency") +
  theme_minimal() +
  ggtitle("Crosstab: Day vs. Time with color gradient")



##------------------REPARTITION DES BASES ----------------------------

# Compter le nombre de trajets pour chaque base
base_counts <- dt %>%
  group_by(Base) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Créer un graphique à barres pour la proportion de chaque base
ggplot(base_counts, aes(x = Base, y = proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of trips by Base", x = "Base", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#------------------REPARTITION DES BASES sur la carte----------------------------

# Créer une palette de couleurs unique pour chaque base
palette_bases <- colorFactor(topo.colors(n_distinct(dt$Base)), dt$Base)

# Filtrer les trajets d'une journée spécifique, ici le 10 septembre
dt_filtered_day <- dt %>%
  filter(date == as.Date("2014-09-10"))

# Vérifier le nombre de trajets pour ce jour
print(paste("Nombre de trajets pour le 10 septembre:", nrow(dt_filtered_day)))

# Créer une carte interactive où chaque base a une couleur unique
m <- leaflet(data = dt_filtered_day) %>%
  addTiles() %>%
  setView(lng = -73.935242, lat = 40.730610, zoom = 12) %>%
  addCircleMarkers(~Lon, ~Lat, color = ~palette_bases(Base), radius = 3, stroke = FALSE, fillOpacity = 0.7,
                   popup = ~paste("Base:", Base, "<br>Date/Heure:", Date.Time))

# Afficher la carte
m


#----------------Repartition des trajets sur la carte en fonction des heures -------------


# Filtrer les trajets entre 7h et 8h le 10 septembre et dans la zone spécifiée
dt_filtered_time_location_7_8 <- dt %>%
  filter(date == as.Date("2014-09-10") & hour >= 7 & hour < 8 )

# Filtrer les trajets entre 17h et 18h le 10 septembre et dans la zone spécifiée
dt_filtered_time_location_17_18 <- dt %>%
  filter(date == as.Date("2014-09-10") & hour >= 17 & hour < 18 )

# Vérifier le nombre de trajets sélectionnés pour les deux plages horaires
print(paste("Nombre de trajets entre 7h et 8h le 10 septembre dans la zone spécifiée:", nrow(dt_filtered_time_location_7_8)))
print(paste("Nombre de trajets entre 17h et 18h le 10 septembre dans la zone spécifiée:", nrow(dt_filtered_time_location_17_18)))

# Créer une carte interactive avec leaflet pour visualiser les points de départ filtrés par temps et location
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -73.935242, lat = 40.730610, zoom = 12) %>%  # Centrer sur New York
  addCircleMarkers(data = dt_filtered_time_location_7_8, ~Lon, ~Lat, radius = 3, color = "blue", stroke = FALSE, fillOpacity = 0.7, 
                   popup = ~paste("Base:", Base, "<br>Date/Heure:", Date.Time)) %>%
  addCircleMarkers(data = dt_filtered_time_location_17_18, ~Lon, ~Lat, radius = 3, color = "red", stroke = FALSE, fillOpacity = 0.7, 
                   popup = ~paste("Base:", Base, "<br>Date/Heure:", Date.Time))

# Afficher la carte
m

#on voit bien les demarcation entre zones residentielle et quartier d'affaires 
#les gens partent au travail le matin et rentre chez eux le soir.

#-------------------------------vendredi soir tard (fete) ------------------------------


# Filtrer les trajets entre 0h et 1h le 14 juillet 2014 et dans la zone spécifiée
dt_filtered_time_location_7_8 <- dt %>%
  filter(date == as.Date("2014-07-14") & hour >= 2 & hour < 3)

# Vérification du nombre de trajets filtrés
print(paste("Nombre de trajets entre 0h et 1h le 14 juillet dans la zone spécifiée:", nrow(dt_filtered_time_location_7_8)))

# Créer une carte interactive avec leaflet pour visualiser les points de départ filtrés par temps et location
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -73.935242, lat = 40.730610, zoom = 12) %>%  # Centrer sur New York
  addCircleMarkers(data = dt_filtered_time_location_7_8, ~Lon, ~Lat, 
                   radius = 3, color = "blue", stroke = FALSE, fillOpacity = 0.7, 
                   popup = ~paste("Base:", Base, "<br>Date/Heure:", Date.Time))

# Afficher la carte
m



## ---------------------------------- Heatmap  ----------------------------


# Créer une heatmap avec l'ensemble des trajets de la ville pour la date donnée
heatmap_all_trips <- leaflet(data = dt[dt$date == as.Date("2014-09-10"), ]) %>%
  addTiles() %>%
  setView(lng = -73.99335, lat = 40.75042, zoom = 12) %>%  # Centrer sur Madison Square Garden
  addHeatmap(lng = ~Lon, lat = ~Lat, blur = 20, max = 0.05, radius = 15)

# Afficher la heatmap sur tous les trajets de la ville
heatmap_all_trips


## ---------------------------------- Heat map avec coords  ----------------------------


# 40.618607, -74.240206
# 40.809485, -73.744362


# Filtrer les données pour ne conserver que les points dans les limites spécifiées
dt_filtered <- dt %>%
  filter(Lon >= -74.240206 & Lon <= -73.744362 & Lat >= 40.618607 & Lat <= 40.809485)

# Regrouper les coordonnées dans une grille
# Choisir une taille de cellule pour la grille (ici 0.01 degré)
dt_filtered <- dt_filtered %>%
  mutate(lat_bin = cut(Lat, breaks = seq(40.618607, 40.809485, by = 0.001)),
         lon_bin = cut(Lon, breaks = seq(-74.240206, -73.744362, by = 0.001)))

# Compter le nombre de points dans chaque case de la grille
heatmap_data <- dt_filtered %>%
  group_by(lat_bin, lon_bin) %>%
  summarise(count = n(), .groups = 'drop')

# Convertir les bins de lat_bin et lon_bin en numéros pour les tracer
heatmap_data <- heatmap_data %>%
  mutate(lat_center = as.numeric(sub("\\(.*,(.*)]", "\\1", lat_bin)),  # Extraire le centre des bins
         lon_center = as.numeric(sub("\\(.*,(.*)]", "\\1", lon_bin)))

# Ajuster la scale_fill_gradientn avec des points intermédiaires
ggplot(heatmap_data, aes(x = lon_center, y = lat_center, fill = count)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("black", "yellow", "orange", "red"), 
                       values = scales::rescale(c(0, 10, 100, max(heatmap_data$count))),
                       limits = c(0, max(heatmap_data$count))) +
  labs(title = "Heatmap of Lat and Lon coordinates", x = "Long", y = "Lat", fill = "Density") +
  theme_minimal()



## ---------------------------------- Clustering  ----------------------------


dt_filtered_day <- dt %>%
  filter(date == as.Date("2014-09-10"))

# Vérifier le nombre de trajets pour ce jour
print(paste("Nombre de trajets pour le 10 septembre:", nrow(dt_filtered_day)))

# Supprimer les valeurs NA dans les coordonnées si nécessaire
dt_filtered_day <- dt_filtered_day %>%
  filter(!is.na(Lat) & !is.na(Lon))

# Sélectionner les colonnes Lat et Lon pour le clustering
coords <- dt_filtered_day %>%
  select(Lat, Lon)

# Appliquer K-means clustering (choisir un nombre de clusters, par exemple 5)
set.seed(123)  # pour avoir des résultats reproductibles
k <- 50  # nombre de clusters, à ajuster en fonction des besoins
kmeans_result <- kmeans(coords, centers = k)

# Ajouter les clusters au dataframe original
dt_filtered_day$cluster <- as.factor(kmeans_result$cluster)

# Créer une palette de couleurs basée sur les clusters (ici, 5 clusters)
palette <- colorFactor(palette = "Set1", domain = dt_filtered_day$cluster)

# Extraire les coordonnées des centroids
centroids <- as.data.frame(kmeans_result$centers)
colnames(centroids) <- c("Lat", "Lon")


# Carte 1 : Trajets avec clusters

m1 <- leaflet(data = dt_filtered_day) %>%
  addTiles() %>%
  setView(lng = -73.935242, lat = 40.730610, zoom = 12) %>%  # Centrer sur New York
  addCircleMarkers(~Lon, ~Lat, color = ~palette(cluster), radius = 3, stroke = FALSE, fillOpacity = 0.7, 
                   popup = ~paste("Base:", Base, "<br>Date/Hour:", Date.Time, "<br>Cluster:", cluster))

# Afficher la première carte (clusters avec trajets)
m1


# Carte 2 : Centroids uniquement

m2 <- leaflet(data = centroids) %>%
  addTiles() %>%
  setView(lng = -73.935242, lat = 40.730610, zoom = 12) %>%  # Centrer sur New York
  addCircleMarkers(~Lon, ~Lat, radius = 8, color = "red", stroke = TRUE, fillOpacity = 1, 
                   popup = ~paste("Centroid: ", rownames(centroids)))

# Afficher la deuxième carte (centroids uniquement)
m2

#________________________________________
# Fonction pour filtrer les trajets dans un rayon donné (Meatpacking District)
filter_within_radius <- function(data, lat_center, lon_center, radius) {
  data <- data %>%
    mutate(Lon = as.numeric(Lon),
           Lat = as.numeric(Lat)) %>%
    filter(!is.na(Lon) & !is.na(Lat))
  
  # Correct the matrix structure for distHaversine
  data <- data %>%
    mutate(distance = distHaversine(
      c(lon_center, lat_center),
      cbind(Lon, Lat)  # Properly combine Lon and Lat into a matrix
    ))
  
  filtered_data <- data %>%
    filter(distance <= radius) %>%
    select(-distance)
  
  return(filtered_data)
}


#----------------------------Calendrier météo (précipitation) / nombre de trajets --------------------------------------------------

# Create the base calendar_data with the date range
calendar_data <- data.frame(
  Date = seq(as.Date("2014-04-01"), as.Date("2014-09-30"), by = "day")
)

# Merge and scale rides in one step, keeping only the 'Date' and 'scaled_rides' columns
calendar_data <- merge(calendar_data, rides_per_day, by.x = "Date", by.y = "date", all.x = TRUE) %>%
  transmute(Date, scaled_rides = rides / max(rides, na.rm = TRUE))

# Now match the precipitation values from avg_weather based on the Date
calendar_data$precipitation <- avg_weather$precipitation[avg_weather$date %in% calendar_data$Date]

# Check the structure of the new calendar_data
str(calendar_data)

# Add weekday and week numbers for plotting the calendar layout
calendar_data <- calendar_data %>%
  mutate(Weekday = wday(Date, label = TRUE),
         Week = week(Date))


# Create the calendar plot with precipitation as a color gradient
ggplot(calendar_data, aes(x = Weekday, y = -Week)) +
  # Use a color gradient for precipitation
  geom_tile(aes(fill = precipitation), color = "white", size = 0.2) +
  # Customize the fill scale with appropriate colors for precipitation levels (low to high)
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Precipitation (mm)") +
  # Customize the plot
  labs(title = "Precipitation Calendar (April - September 2014)", fill = "Precipitation") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

# Create the calendar plot using scaled rides (0 to 1 scale)
ggplot(calendar_data, aes(x = Weekday, y = -Week)) +
  # Use a color gradient for scaled rides (0 to 1)
  geom_tile(aes(fill = scaled_rides), color = "white", size = 0.2) +
  # Customize the fill scale with appropriate colors for scaled rides
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Scaled Rides per Day (0-1)") +
  # Customize the plot
  labs(title = "Scaled Number of Rides Calendar (April - September 2014)", fill = "Scaled Rides") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())



#----------------------------Meatpacking District(quartier de la fete) --------------------------------------------------

# Dates spécifiques à filtrer
date_july <- as.Date("2014-07-04") #jour de fete national
date_june <- as.Date("2014-07-05")

# Filtrer les trajets pour chaque date
dt_filtered_july <- dt[dt$date == date_july, ]
dt_filtered_june <- dt[dt$date == date_june, ]

# Filtrer les trajets autour du Meatpacking District pour chaque date
meatpacking_rides_july <- filter_within_radius(dt_filtered_july, 40.740550952796674, -74.0077867793809, 1000) %>%
  mutate(hour = lubridate::hour(Date.Time), date = "4 juillet")

meatpacking_rides_june <- filter_within_radius(dt_filtered_june, 40.740550952796674, -74.0077867793809, 1000) %>%
  mutate(hour = lubridate::hour(Date.Time), date = "4 juin")

# Combiner les données
combined_rides <- bind_rows(meatpacking_rides_july, meatpacking_rides_june)

# Compter les trajets par heure pour chaque date
hourly_counts_combined <- combined_rides %>%
  group_by(hour, date) %>%
  summarise(count = n())

# Créer un graphique avec ggplot2 pour visualiser les trajets par heure pour les deux dates
ggplot(hourly_counts_combined, aes(x = hour, y = count, color = date, group = date)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Nombre de trajets par heure (Meatpacking District)",
       x = "Heure",
       y = "Nombre de trajets") +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

#Grosse hausse lors de la fete national


#---------------------------- ANALYSE US OPEN --------------------------------------------------

# Date spécifique à filtrer
us_open_date <- as.Date("2014-09-03")

# Filtrer les trajets pour la date spécifique
dt_filtered_us <- dt[dt$date == us_open_date, ]

# Filtrer les trajets dans un rayon de 1000 mètres autour des coordonnées de l'US Open
us_open_rides <- filter_within_radius(dt_filtered_us, 40.74946, -73.84618, 1000)

# Extraire l'heure des trajets et les regrouper par heure
us_open_rides$heure <- hour(us_open_rides$Date.Time)  # Extraire l'heure à partir de la colonne 'Date.Time'

# Compter le nombre de trajets par heure
trajets_par_heure <- us_open_rides %>%
  group_by(heure) %>%
  summarise(nombre_trajets = n())

# Afficher un graphique du nombre de trajets par heure
ggplot(trajets_par_heure, aes(x = heure, y = nombre_trajets)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +  # Afficher chaque heure de la journée
  labs(title = "Nombre de trajets par heure autour de l'US Open (3 septembre 2014)",
       x = "Heure", y = "Nombre de trajets") +
  theme_minimal()

# Afficher le nombre de trajets dans cette zone
nombre_trajets <- nrow(us_open_rides)
print(paste("Nombre de trajets le", us_open_date, "dans la zone spécifiée:", nombre_trajets))

# Créer une carte centrée sur le Court de l'US open
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -73.84618, lat = 40.74946, zoom = 12)

# Ajouter les trajets autour du US Open
m <- m %>%
  addCircleMarkers(data = us_open_rides,
                   lng = ~Lon,
                   lat = ~Lat,
                   radius = 3,
                   color = "red",
                   stroke = FALSE,
                   fillOpacity = 0.7,
                   popup = ~paste("Base:", Base, "<br>Date/Heure:", Date.Time))

# Afficher la carte
m

# --------------Courbe du nombre de trajets sur la periode de l'US Open -------------------------

# Plage de dates spécifique à analyser (mi-août à fin septembre)
start_date <- as.Date("2014-08-15")
end_date <- as.Date("2014-09-30")

# Filtrer les trajets pour la plage de dates spécifique
dt_filtered_period <- dt[dt$date >= start_date & dt$date <= end_date, ]

# Filtrer pour les trajets autour du lieu de l'US Open (Flushing Meadows)
us_open_rides_period <- filter_within_radius(dt_filtered_period, 40.74946, -73.84618, 1000)

# Compter les trajets par jour
daily_counts <- us_open_rides_period %>%
  group_by(date) %>%
  summarise(count = n())

# Créer une courbe avec ggplot2 pour visualiser les trajets pendant l'US Open
ggplot(daily_counts, aes(x = date, y = count)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Nombre de trajets par jour (US Open, mi-août à fin septembre)",
       x = "Date",
       y = "Nombre de trajets") +
  scale_x_date(date_labels = "%d-%m-%Y", date_breaks = "1 day") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.title = element_text(hjust = 0.5))

#3 pics : 
# - pic1 : Roger Federer and Serena Williams (quart) qui jouent
# - pic2 : Nadal et Williams (demi)
# - pic3 : final femme avec Serena Williams

#------------------------------- ANALYSE Madison Square Garden -----------------------------------

# # Filtrer les trajets pour la date du 15 juillet 2014 autour de Madison Square Garden
madison_rides_event <- filter_within_radius(dt[dt$date == as.Date("2014-07-15"), ],
                                            lat_center = 40.75042, lon_center = -73.99335, radius = 300)

# Vérifier le nombre de trajets sélectionnés
print(paste("Nombre de trajets le 15 juillet autour de Madison Square Garden:", nrow(madison_rides_event)))

# Créer une carte interactive avec leaflet pour visualiser les trajets
m_event <- leaflet(data = madison_rides_event) %>%
  addTiles() %>%
  setView(lng = -73.99335, lat = 40.75042, zoom = 14) %>%  # Centrer sur Madison Square Garden
  addCircleMarkers(~Lon, ~Lat, radius = 3, color = "red", stroke = FALSE, fillOpacity = 0.7,
                   popup = ~paste("Base:", Base, "<br>Date/Heure:", Date.Time))

# Afficher la carte interactive
m_event

# Filtrer les trajets pour le mois de juillet 2014 autour de Madison Square Garden
july_rides_madison <- dt[dt$date >= as.Date("2014-07-01") & dt$date <= as.Date("2014-07-31"), ]
madison_rides_july <- filter_within_radius(july_rides_madison, lat_center = 40.75042, lon_center = -73.99335, radius = 300)

# Compter le nombre de trajets par jour
trajets_par_jour <- madison_rides_july %>%
  group_by(date) %>%
  summarise(nombre_trajets = n())

# Afficher un graphique du nombre de trajets par jour pendant le mois de juillet
ggplot(trajets_par_jour, aes(x = date, y = nombre_trajets)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Nombre de trajets par jour autour de Madison Square Garden (Juillet 2014)",
       x = "Date", y = "Nombre de trajets") +
  scale_x_date(date_breaks = "1 day", date_labels = "%d-%m") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#------------------------------- PREDICTION Madison Square Garden -----------------------------------
# Charger les données des événements
events_file <- "~/Downloads/Projet/Event_madison_garden.csv"
dt_events <- read.csv(events_file, stringsAsFactors = FALSE)

# Convertir la colonne `Date` en format Date
dt_events$date <- as.Date(dt_events$date, format="%m/%d/%y")

# Afficher un aperçu des données
head(dt_events)
summary(dt_events)

# Filtrer les événements de 2015
events_2015 <- dt_events %>%
  filter(format(date, "%Y") == "2015")

# Filtrer le nombre de trajets par jour autour de Madison Square Garden
daily_rides_madison <- dt[dt$date >= as.Date("2014-03-31") & dt$date <= as.Date("2014-09-30"), ]
madison_rides <- filter_within_radius(daily_rides_madison, lat_center = 40.75042, lon_center = -73.99335, radius = 300)

# Compter le nombre de trajets par jour
madison_total_rides <- madison_rides %>%
  group_by(date) %>%
  summarise(nombre_trajets = n())

# Fusionner avec daily_rides pour les événements ayant déjà des trajets
merged_data <- dt_events %>%
  left_join(madison_total_rides, by = "date") %>%
  filter(format(date, "%Y") == "2014")

# Convertir Attendance et Box.Office en format numérique
merged_data$Attendance <- as.numeric(gsub(",", "", merged_data$Attendance))
merged_data$Box.Office <- as.numeric(gsub("[$,]", "", merged_data$Box.Office))

# Créer le modèle avec les variables indicatrices pour Nationality
model <- lm(nombre_trajets ~ Attendance + Box.Office, data = merged_data)

# Prédire le nombre de trajets pour toutes les données
merged_data$predicted_rides <- predict(model, newdata = merged_data)

# Fusionner les événements de 2014 et 2015 pour la visualisation
events <- dt_events %>%
  left_join(madison_total_rides, by = "date") %>%
  mutate(predicted_rides = ifelse(format(date, "%Y") == "2015", merged_data$predicted_rides, NA))

# Créer le graphique
ggplot(events, aes(x = date, y = Attendance)) +
  geom_point(aes(size = ifelse(is.na(nombre_trajets), predicted_rides, nombre_trajets), color = "nombre_trajets"), alpha = 0.6) +
  scale_size_continuous(name = "Nombre de trajets", range = c(2, 10)) + 
  scale_color_manual(values = c("nombre_trajets" = "red")) + 
  labs(title = "Événements 2014-2015 au Madison Square Garden",
       x = "Date",
       y = "Assistance (Attendance)",
       size = "Nombre de trajets") +
  theme_minimal()