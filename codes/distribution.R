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

lelez <- coursdeau_metro[coursdeau_metro$NomEntiteH == "Le lez",]

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = lelez, size = 0.4, col = "darkblue")

# on rajoute les données de présence 

signes <- read_xlsx("data/Indices.xlsx")
signes$Coord_X <- as.numeric(signes$Coord_X)
signes$Coord_Y <- as.numeric(signes$Coord_Y)
signes_sf <- st_as_sf(as_tibble(signes[,c("Coord_X", "Coord_Y")]), 
                      coords = c("Coord_X", "Coord_Y"), 
                      crs = 4326)
signes_sf <- signes_sf %>% st_transform(crs = st_crs(metropole))

# on visualise
  
ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = lelez, col = "darkblue", lwd = 1) +
  geom_sf(data = signes_sf, size = 2.5, color = "brown")

# je vire les observations pas sur le Lez

# on applique un buffer de 200m sur la trame bleue
lelez_buffer <- st_buffer(lelez, dist = 200, endCapStyle = "SQUARE", nQuadSegs = 5)

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = lelez_buffer, fill = "blue") + 
  geom_sf(data = lelez, fill = "lightblue")

# on ne prend que les points dans ce buffer

signes_lez <- st_filter(signes_sf, lelez_buffer)

# on visualise à nouveau

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = lelez, col = "darkblue", lwd = 1) +
  geom_sf(data = signes_lez, size = 2.5, color = "brown")

# tout bon ! 

#-------------- 2. les données de pièges photos

# contours de la métropole 
# https://data.montpellier3m.fr/dataset/contours-des-communes-de-montpellier-mediterranee-metropole
metropole <- st_read("shp/MMM_MMM_Limites.shp")

# cours d'eau 
# https://geo.data.gouv.fr/fr/datasets/ee5c709c9b7ff928ab2529b79ce6e879c4de6950
coursdeau <- st_read("shp/CoursEau_FXX.shp")

coursdeau_metro <- coursdeau %>% st_intersection(metropole)

lelez <- coursdeau_metro[coursdeau_metro$NomEntiteH == "Le lez",]

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = lelez, size = 0.4, col = "darkblue")


# les pièges

pos_pieges <- st_read("shp/piege_photos_cefe/piege_photos_cefe.shp")

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = lelez, size = 0.8, col = "darkblue") +
  geom_sf(data = pos_pieges, size = 2, color = "brown") +
  geom_sf_label_repel(data = pos_pieges, aes(label = num_piege),
                      force = 100, nudge_x = -2, seed = 10)


# les détections de loutres

det_pieges <- read_xlsx("data/Analyse_sans_rep.xlsx", col_types = "text") %>%
  mutate(num_piege = as.numeric(`N° de piège`))

loutres <- det_pieges %>% filter(`Espèce / Activité` == "Loutre")

# on joint les deux jeux de données
pieges_loutres <- loutres %>% full_join(pos_pieges) %>% st_sf

# les pièges auxquels on a détecté la loutre
options(ggrepel.max.overlaps = Inf)
ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = lelez, size = 0.8, col = "darkblue") +
  geom_sf(data = pieges_loutres, size = 2, color = "brown") +
  geom_sf_label_repel(data = pieges_loutres, aes(label = num_piege),
                      force = 100, nudge_x = -2, seed = 10)







