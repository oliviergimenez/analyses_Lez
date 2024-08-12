library(tidyverse)
theme_set(theme_light(base_size = 18))
library(sf)
library(readxl)
library(ggsflabel)
library(cowplot)

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

studyarea <- ggplot() + 
  geom_sf(data = metropole, fill = "white", lwd = 0.2) + 
  geom_sf(data = bvlez, 
          aes(color = NomEntiteH), lwd = 3, key_glyph = draw_key_timeseries) +
  #scale_color_manual(name = NULL,
  #                   breaks = c("Le Lez", "La Mosson", "La Lironde", "Le Lirou", "Le Verdanson"),
  #                   values = rev(c("#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B")),
  #                   guide = guide_legend(override.aes = list(linetype = rep(1, 5)))) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "grey75"),
        legend.text = element_text(size = 20, face = "bold"),
        legend.key.size = unit(3,"line")) +
  ggspatial::annotation_scale(location = "br", 
                              width_hint = 0.4) +
  ggspatial::annotation_north_arrow(location = "br", 
                                    which_north = "true",
                                    height = unit(0.3, "in"), 
                                    width = unit(0.3, "in"),
                                    pad_x = unit(0.1, "in"), 
                                    pad_y = unit(0.3, "in")) +
  geom_sf(data = centroids %>% 
            filter(nom %in% villes)) +
  geom_sf_label_repel(data = centroids %>% 
                        filter(nom %in% villes),
                      aes(label = nom, x = X, y = Y), 
                      size = 7, 
                      fontface = "bold",
                      alpha = 0.8) +
  labs(x = NULL, y = NULL)
studyarea

fr_df <- st_read("shp/departement.shp")
plot_fr <- fr_df %>%
  ggplot() + 
  geom_sf(color = "black", fill = "white", lwd = .1) +
  annotate(geom = "rect", 
           ymax = st_bbox(metropole)[4], 
           ymin = st_bbox(metropole)[2], 
           xmax = st_bbox(metropole)[3], 
           xmin = st_bbox(metropole)[1], 
           colour = "red", 
           fill = "red",
           lwd = 0.5) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) 
plot_fr

map_with_inset <-
  ggdraw() +
  draw_plot(studyarea) +
  draw_plot(plot_fr, hjust = -0.8, vjust = -5.5, width = 0.15, height = 0.15)
  #draw_plot(plot_fr, x = 770053.4, y = 6286947.7, width = 0.25, height = 0.25) # st_bbox(metropole)
map_with_inset
ggsave("outputs/studyarea.png", dpi = 600, width = 15, height = 15)
