library(tidyverse)
theme_set(theme_light(base_size = 18))
library(sf)
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
library(mapview)

## 1. Cours d'eau 

# contours de la métropole 
# https://data.montpellier3m.fr/dataset/contours-des-communes-de-montpellier-mediterranee-metropole
metropole <- st_read("shp/MMM_MMM_Limites.shp")

# cours d'eau 
# https://geo.data.gouv.fr/fr/datasets/ee5c709c9b7ff928ab2529b79ce6e879c4de6950
coursdeau <- st_read("shp/CoursEau_FXX.shp")
coursdeau_metro <- coursdeau %>% st_intersection(metropole)

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = coursdeau_metro, size = 0.4, col = "darkblue")
ggsave("outputs/coursdeaumetro.png", dpi = 600, width = 15, height = 15)

## 2. Occupation des sols

# données ici <https://data.montpellier3m.fr/dataset/evolution-de-loccupation-du-sol-de-montpellier-mediterranee-metropole/resource/9ad7fac7-40e5>
sol <- st_read("shp/MMM_MMM_OccupationSol.shp")
sol <- sol %>% st_intersection(metropole)

# milieux
unique(sol$milieu2019)

# nomenclature
unique(sol$lib19_niv1)

# visualise

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = sol, aes(fill = milieu2019)) +
  scale_fill_manual(values = c('yellow','grey90','lightgreen'),
                    name = "Occupation des sols",
                    labels = unique(sol$milieu2019))
ggsave("outputs/occsol1.png", dpi = 600, width = 15, height = 15)

sol_viz <- sol %>%
  mutate(
    couleur = case_when(
      lib19_niv1 == "Espaces agricoles" ~ "yellow",
      lib19_niv1 == "Espaces urbanisés" ~ "grey90",
      lib19_niv1 == "Espaces récréatifs" ~ "grey90",
      lib19_niv1 == "Surfaces industrielles ou commerciales et infrastructures de communication" ~ "grey90",
      lib19_niv1 == "Espaces naturels non boisés" ~ "lightgreen",
      lib19_niv1 == "Extraction de matériaux, décharges, chantiers" ~ "grey90",
      lib19_niv1 == "Espaces boisés" ~ "green",
      lib19_niv1 == "Eau" ~ "blue"))

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = sol_viz, aes(fill = couleur)) +
  scale_fill_manual(values = c("blue", "green", "grey90", "lightgreen", "yellow"),
                    name = "Occupation des sols",
                    labels = c("Eau", 
                               "Espaces boisés", 
                               "Espaces urbanisés, récréatifs et artificialisés",
                               "Espaces naturels non boisés",
                               "Espaces agricoles"))

ggsave("outputs/occsol2.png", dpi = 600, width = 15, height = 15)


## 3. Trame verte

# On récupère les données sur la trame verte depuis le site INPN / SRCE <https://inpn.mnhn.fr/programme/trame-verte-et-bleue/donnees-srce> : 
  
trame_verte <- st_read("shp/N_SRCE_RESERVOIR_S_000.shp")

# On intersecte avec les limites de la métro : 
  
tv_montpellier <- trame_verte %>% st_intersection(metropole)

# Sur une carte, ça donne : 
  
ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = tv_montpellier, col = "green")
ggsave("outputs/trameverte.png", dpi = 600, width = 15, height = 15)

## 4. Trame bleue

# On récupère les données sur la trame bleue depuis le site INPN / SRCE <https://inpn.mnhn.fr/programme/trame-verte-et-bleue/donnees-srce> : 
  
trame_bleue <- st_read("shp/N_SRCE_COURS_EAU_S_000.shp")

# On intersecte avec les limites de la métro : 
  
tb_montpellier <- trame_bleue %>% st_intersection(metropole)

# Sur une carte, ça donne :
ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = tb_montpellier, col = "blue")
ggsave("outputs/tramebleue.png", dpi = 600, width = 15, height = 15)

# On applique un buffer de 500m sur la trame bleue

tb_montpellier_buffer <- st_buffer(x = tb_montpellier, 
                                   dist = 500, 
                                   endCapStyle = "SQUARE", 
                                   nQuadSegs = 5)

# Et on vérifie que tout s'est bien passé :

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = tb_montpellier_buffer, fill = "blue") + 
  geom_sf(data = tb_montpellier, fill = "lightblue")
ggsave("outputs/buffer.png", dpi = 600, width = 15, height = 15)

## 5. Trame turquoise

# On récupère les types de sol fav à la loutre (déplacements, installation, repro): 

mask <- sol$c2019_niv4 == 5400 | # Marais salants
sol$c2019_niv4 == 6110 | # Feuillus mésophiles dominants
sol$c2019_niv4 == 6120 | # Peupleraies
sol$c2019_niv4 == 6130 | # Feuillus sclérophylles dominants
sol$c2019_niv4 == 6200 | # Conifères dominants
sol$c2019_niv4 == 6310 | # Peuplements mixtes conifères-feuillus mésophiles
sol$c2019_niv4 == 6320 | # Peuplements mixtes conifères-feuillus sclérophylles
sol$c2019_niv4 == 6400 | # Coupes forestières et jeunes plantations
sol$c2019_niv4 == 6510 | # Haies
sol$c2019_niv4 == 6520 | # Boisements ripicoles linéaires
sol$c2019_niv4 == 6530 | # Alignements d'arbres
sol$c2019_niv4 == 7810 | # Marais intérieurs
sol$c2019_niv4 == 7820 | # Marais maritimes
sol$c2019_niv4 == 8110 | # Cours d'eau
sol$c2019_niv4 == 8120 | # Canaux
sol$c2019_niv4 == 8210 | # Plans d’eau douce
sol$c2019_niv4 == 8220 | # Plans d'eau et lagunes littorales
sol$c2019_niv4 == 8300 # Mer, océan et estuaires

ripysilves <- sol[mask,]

# Et dans la trame bleue, on regarde le milieu favorable à la loutre :
  
tt_montpellier <- tb_montpellier_buffer %>% st_intersection(ripysilves)

# Ca donne sur une carte :
  
ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = tt_montpellier, fill = "purple", lwd = .3)
ggsave("outputs/trameturquoise.png", dpi = 600, width = 15, height = 15)

# On rajoute les données de présence 

signes <- readxl::read_xlsx("data/Indices.xlsx")
signes$Coord_X <- as.numeric(signes$Coord_X)
signes$Coord_Y <- as.numeric(signes$Coord_Y)
signes_sf <- st_as_sf(as_tibble(signes[,c("Coord_X", "Coord_Y")]), 
                      coords = c("Coord_X", "Coord_Y"), 
                      crs = 4326)
signes_sf <- signes_sf %>% st_transform(crs = st_crs(metropole))

# On visualise :
  
ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = tt_montpellier, fill = "purple", lwd = .3) + 
  geom_sf(data = coursdeau_metro, col = "blue", lwd = .3) +
  geom_sf(data = signes_sf, size = .5, color = "brown")

## 6. Est-ce que chaque indice de présence (points) est dans la trame turquoise (polygones)

# Joint les deux objets spatiaux
dim(signes_sf)
dim(tt_montpellier)
joined <- st_join(signes_sf, tt_montpellier, join = st_within)
dim(joined)

# Crée une nouvelle colonne pour indiquer si un indice est dans un polygone de la trame turquoise ou pas
joined <- joined %>%
  mutate(is_within = !is.na(metropole))

dedans_points <- joined %>% filter(is_within == TRUE)
dehors_points <- joined %>% filter(is_within == FALSE)

dim(dedans_points)
dim(dehors_points)

points <- rbind(dedans_points, dehors_points)
points$ou <- c(rep("dedans", dim(dedans_points)[1]),
               rep("dehors", dim(dehors_points)[1]))

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = tt_montpellier, fill = "purple", lwd = .3) + 
  geom_sf(data = coursdeau_metro, col = "blue", lwd = .3) +
  geom_sf(data = points, size = 2.5, aes(color = ou)) +
  scale_color_manual(name = "Dans la trame turquoise?", 
                     values = c("dedans" = "orange",
                                "dehors" = "gray"),
                     labels = c("Oui","Non")) + 
  theme(legend.position = "bottom")
ggsave("outputs/altogether.png", dpi = 600, width = 15, height = 15)

