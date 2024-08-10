library(tidyverse)
theme_set(theme_light(base_size = 18))
library(sf)
library(readxl)
library(ggsflabel)
library(cowplot)

# contours de la métropole 
# https://data.montpellier3m.fr/dataset/contours-des-communes-de-montpellier-mediterranee-metropole
metropole <- st_read("shp/MMM_MMM_Limites.shp")

# cours d'eau données EPTB
coursdeau <- st_read("shp/SYBLE_BVLEZ_CoursEau.shp") %>%
  st_zm() %>%
  st_transform(st_crs(metropole))
coursdeau_metro <- coursdeau %>% st_intersection(metropole)

#unique(coursdeau_metro$NOM[str_detect(coursdeau_metro$NOM, "erd")])

lez <- coursdeau_metro[coursdeau_metro$NOM == "le Lez",]
lez$NOM <- fct_recode(lez$NOM, 'Le Lez' = 'le Lez')
mosson <- coursdeau_metro[coursdeau_metro$NOM == "la Mosson",]
lironde <- coursdeau_metro[coursdeau_metro$NOM == "la Lironde",]
lirou <- coursdeau_metro[coursdeau_metro$NOM == "le Lirou",]
verdanson <- coursdeau_metro[coursdeau_metro$NOM == "le Verdanson",]
bvlez <- rbind(lez,
               mosson,
               lironde,
               lirou,
               verdanson)
bvlez <- bvlez[!is.na(bvlez$NOM),]

metropole$nom <- fct_recode(metropole$nom, 
                            'Montpellier' = 'MONTPELLIER',
                            'Castelnau-le-Lez' = 'CASTELNAU LE LEZ',
                            'Montferrier-sur-Lez' = 'MONTFERRIER-SUR-LEZ',
                            'Clapiers' = 'CLAPIERS',
                            'Prades-le-Lez' = 'PRADES-LE-LEZ',
                            'Lattes' = 'LATTES',
                            "Saussan" = "SAUSSAN",              
                            "Fabrègues" = "FABREGUES",               
                            "Juvignac" = "JUVIGNAC",                
                            "Saint-Jean-de-Védas" = "SAINT-JEAN-DE-VEDAS",     
                            "Villeneuve-les-Maguelone" = "VILLENEUVE-LES-MAGUELONE",
                            "Grabels" = "GRABELS",             
                            "Lavérune" = "LAVERUNE")

#villes <- c('Castelnau-le-Lez', 'Montferrier-sur-Lez', 'Clapiers', 'Prades-le-Lez', 'Lattes','Montpellier',"Saussan" ,"Fabrègues" , "Juvignac" , "Saint-Jean-de-Védas" , "Villeneuve-les-Maguelone" , "Grabels", "Lavérune")
villes <- c('Castelnau-le-Lez', 'Montferrier-sur-Lez', 'Prades-le-Lez', 'Lattes','Montpellier',"Villeneuve-les-Maguelone")
centroids <- metropole %>% st_centroid()
coordinates <- centroids %>% st_coordinates()
centroids$X <- coordinates[,1]
centroids$Y <- coordinates[,2]

# RColorBrewer::brewer.pal(9, "Blues")

ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = bvlez, 
          aes(color = NOM), lwd = 1, key_glyph = draw_key_timeseries) 
