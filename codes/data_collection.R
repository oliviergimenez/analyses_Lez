library(tidyverse)
theme_set(theme_light(base_size = 15))
library(sf)
library(readxl)
library(ggsflabel)


#-------------- 1. les données de presence-only

# contours de la métropole 
# https://data.montpellier3m.fr/dataset/contours-des-communes-de-montpellier-mediterranee-metropole
metropole <- st_read("shp/MMM_MMM_Limites.shp")

# cours d'eau 
# https://geo.data.gouv.fr/fr/datasets/ee5c709c9b7ff928ab2529b79ce6e879c4de6950
coursdeau <- st_read("shp/CoursEau_FXX.shp")

coursdeau_metro <- coursdeau %>% st_intersection(metropole)

lez <- coursdeau_metro[coursdeau_metro$NomEntiteH == "Le lez",]
lez$NomEntiteH <- fct_recode(lez$NomEntiteH, 'Le Lez' = 'Le lez')
mosson <- coursdeau_metro[coursdeau_metro$NomEntiteH == "La Mosson",]
lironde <- coursdeau_metro[coursdeau_metro$NomEntiteH == "La Lironde",]
lirou <- coursdeau_metro[coursdeau_metro$NomEntiteH == "Le Lirou",]
verdanson <- coursdeau_metro[coursdeau_metro$NomEntiteH == "Le Verdanson",]
bvlez <- rbind(lez,
               mosson,
               lironde,
               lirou,
               verdanson)
bvlez_buffer <- st_buffer(bvlez, dist = 200, endCapStyle = "SQUARE", nQuadSegs = 5)

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = bvlez, size = 0.4, col = "darkblue")

#-------------- 1. les sites de recherche d'épreintes/empreintes

signes <- read_xlsx("data/Indices.xlsx", sheet = 1)
signes$Coord_X <- as.numeric(signes$Coord_X)
signes$Coord_Y <- as.numeric(signes$Coord_Y)
signes_sf <- st_as_sf(as_tibble(signes[,c("Coord_X", "Coord_Y")]), 
                      coords = c("Coord_X", "Coord_Y"), 
                      crs = 4326)
signes_sf <- signes_sf %>% 
  st_transform(crs = st_crs(metropole)) %>%
  st_intersection(metropole) %>% # obs dans la metropole
  st_filter(bvlez_buffer) # obs dans un buffer de 200m autour des cours d'eau
  
ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = bvlez, col = "blue", lwd = 1) +
  ggimage::geom_image(data = signes_sf, 
                      aes(x = st_coordinates(signes_sf)[,1], 
                          y = st_coordinates(signes_sf)[,2],
                          image = "img/poop-emoji.png")) +
  xlab("") + 
  ylab("")
# geom_sf(data = signes_sf, size = 5, color = "brown", shape = 16)


ggsave("outputs/spraints.png", dpi = 600, width = 15, height = 15)


#-------------- 2. les données de pièges photos

pos_pieges <- st_read("shp/piege_photos_loutre_cefe.shp")
length(pos_pieges$Numero)

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = bvlez, lwd = 1, col = "blue") +
  ggimage::geom_image(data = pos_pieges, 
                      aes(x = st_coordinates(pos_pieges)[,1], 
                          y = st_coordinates(pos_pieges)[,2],
                          image = "img/camera-emoji.png")) +
  xlab("") + 
  ylab("")
#  geom_sf(data = pos_pieges, size = 5, color = "black", shape = 16) #+

ggsave("outputs/camptraps.png", dpi = 600, width = 15, height = 15)

#  geom_sf_label_repel(data = pos_pieges, 
#                      aes(label = num_piege),
#                      force = 100, 
#                      nudge_x = -2, 
#                      seed = 10) +
#  xlab("") + 
#  ylab("")

#-------------- 3. les points de pompage ADNe

adne <- read_csv("data/ADNe/résultats_campagne_adne_lez_novembre_d_cembre_2023.csv")

adne_sf <- st_as_sf(as_tibble(adne[,c("Longitude", "Latitude")]), 
                      coords = c("Longitude", "Latitude"), 
                      crs = 4326) %>%
  st_transform(st_crs(bvlez))


ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = bvlez, col = "blue", lwd = 1) +
  ggimage::geom_image(data = adne_sf, 
                    aes(x = st_coordinates(adne_sf)[,1], 
                        y = st_coordinates(adne_sf)[,2],
                        image = "img/dna-emoji.png")) +
  xlab("") + 
  ylab("")
#  geom_sf(data = adne_sf, size = 5, color = "green", shape = 16)
ggsave("outputs/adne.png", dpi = 600, width = 15, height = 15)

#-------------- 4. les points de prélèvements ADN pour structuration génétique/régime alimentaire
