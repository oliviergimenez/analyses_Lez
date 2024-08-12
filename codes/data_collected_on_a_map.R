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

#-------------- 1. les épreintes/empreintes

signes <- read_xlsx("data/Indices.xlsx", sheet = 1)
signes$Coord_X <- as.numeric(signes$Coord_X)
signes$Coord_Y <- as.numeric(signes$Coord_Y)
signes_sf <- st_as_sf(as_tibble(signes[,c("Coord_X", "Coord_Y")]), 
                      coords = c("Coord_X", "Coord_Y"), 
                      crs = 4326)
sites_sf <- signes_sf %>% 
  st_transform(crs = st_crs(metropole)) %>%
  st_intersection(metropole) %>% # obs dans la metropole
  st_filter(bvlez_buffer) # obs dans un buffer de 200m autour des cours d'eau
  


signes <- read_xlsx("data/Indices.xlsx", sheet = 2)
signes$geom <- str_replace(string = signes$wkt_geom,
                           pattern = "Point", 
                           replacement = "")
signes$geom <- str_replace(string = signes$geom,
                           pattern = " \\(", 
                           replacement = "")
signes$geom <- str_replace(string = signes$geom,
                           pattern = "\\)", 
                           replacement = "")
signes <- signes %>%
  separate_wider_delim(geom, 
                       delim = " ", 
                       names = c("Coord_X", "Coord_Y"))
signes$Coord_X <- as.numeric(signes$Coord_X)
signes$Coord_Y <- as.numeric(signes$Coord_Y)
signes_sf <- st_as_sf(as_tibble(signes[,c("Coord_X", "Coord_Y")]), 
                      coords = c("Coord_X", "Coord_Y"), 
                      crs = 4326)
signes_sf <- signes_sf %>% 
  st_transform(crs = st_crs(metropole)) %>%
  st_intersection(metropole) %>% # obs dans la metropole
  st_filter(bvlez_buffer) # obs dans un buffer de 200m autour des cours d'eau

# nb total d'indices trouvés
nrow(signes_sf)

# combien d'indices dans chaque transect?
sites_buffer <- st_buffer(sites_sf, 
                          dist = 2050, 
                          endCapStyle = "SQUARE", 
                          nQuadSegs = 5)

nb_indices <- lengths(st_intersects(sites_buffer, signes_sf))
sites_sf$nb_indices <- nb_indices

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = bvlez, col = "blue", lwd = 1) +
  geom_sf(data = sites_sf, col = "orange", size = 7) + 
  geom_text(data = sites_sf, 
            aes(x = st_coordinates(sites_sf)[,1], 
                                 y = st_coordinates(sites_sf)[,2],
                                 label = nb_indices), 
            colour = "white") +
  xlab("") + 
  ylab("")

ggsave("outputs/data_spraints.png", dpi = 600, width = 15, height = 15)


#-------------- 2. les données de pièges photos

pos_pieges <- st_read("shp/piege_photos_cefe.shp")

videos <- readxl::read_xlsx("data/pieges_photo_classes.xlsx") %>%
  filter(`Espèce / Activité` == "Loutre") %>%
  mutate(num_piege = `N° de piège`) %>%
  select(num_piege) %>%
  count(num_piege)
  
pos_pieges <- pos_pieges %>% 
  left_join(videos) %>%
  mutate(nb_camtramps = n) %>%
  mutate(nb_camtramps = if_else(!is.na(nb_camtramps), nb_camtramps, 0))

sum(pos_pieges$nb_camtramps)

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = bvlez, col = "blue", lwd = 1) +
  geom_sf(data = pos_pieges, col = "orange", size = 7) + 
  geom_sf_label_repel(data = pos_pieges, 
                      aes(label = nb_camtramps),
                      force = 100, 
                      nudge_x = -2, 
                      seed = 10) +
  xlab("") + 
  ylab("")

ggsave("outputs/data_camptraps.png", dpi = 600, width = 15, height = 15)


#-------------- 3. les points de pompage ADNe

adne <- read_csv("data/ADNe/résultats_campagne_adne_lez_novembre_d_cembre_2023.csv")

nb_adne <- c( 
"1/1",
"1/1",
"1/1",
"0/3",
"0/3",
"0/3",
"3/3",
"2/3",
"2/3",
"1/3")

adne_sf <- st_as_sf(as_tibble(adne[,c("Longitude", "Latitude")]), 
                      coords = c("Longitude", "Latitude"), 
                      crs = 4326) %>%
  st_transform(st_crs(bvlez))

adne_sf$nb_adne <- nb_adne

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = bvlez, col = "blue", lwd = 1) +
  geom_sf(data = adne_sf, col = "orange", size = 7) + 
  geom_text(data = adne_sf, 
            aes(x = st_coordinates(adne_sf)[,1], 
                y = st_coordinates(adne_sf)[,2],
                label = nb_adne), 
            colour = "white") +
  xlab("") + 
  ylab("")

ggsave("outputs/data_adne.png", dpi = 600, width = 15, height = 15)

