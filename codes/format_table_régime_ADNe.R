library(rvest)
library(tidyverse)
library(rlist)
library(flextable)
library(officer)

# on récupère la liste des poissons depuis Wikipedia
# https://fr.wikipedia.org/wiki/Liste_des_poissons_d%27eau_douce_en_France_m%C3%A9tropolitaine

url <- "https://fr.wikipedia.org/wiki/Liste_des_poissons_d%27eau_douce_en_France_m%C3%A9tropolitaine"

tmp <- url %>%
  read_html() %>%
  html_nodes("body #content #bodyContent #mw-content-text .mw-parser-output table") %>%
  length() %>%
  as.character()

xpathall <- c()
for (i in 1:as.numeric(tmp)){
  xpathall <- list.append(xpathall, paste0('//*[@id="mw-content-text"]/div/table[', i, ']'))
}

res <- NULL
for (i in 3:32){
  df <- url %>%
    read_html() %>%
    html_nodes(xpath = xpathall[i]) %>%
    html_table(fill = TRUE)
  unlist_df <- df[[1]]
  noms <- unlist_df[,1]
  colnames(noms) <- "nom_de_colonne"
  sep_noms <- noms %>%
    separate_wider_delim(
      "nom_de_colonne",
      delim = "\n",
      names = c("Nom vulgaire", "Nom binomial", "Auteur"))
  res <- rbind(res, sep_noms)
}

res

# ajoute des parenthèses à l'auteur si pas présentes
res <- res %>%
  mutate(Auteur = if_else(!str_detect(Auteur, pattern = "\\("),
                          paste0(paste0("(", Auteur, ")")),
                          Auteur))
res

# sauve tout ça dans un fichier
writexl::write_xlsx(x = res, path = "data/liste_poissons.xlsx")


# certains taxons sont identifiés au genre ou à la famille avec la base de référence SPYGEN

exceptions <- readxl::read_xlsx("data/ADNe/exception_base_de_ref.xlsx") %>%
  mutate(nom_binomial = `Nom scientifique affiché sur les rapports`,
         nom_binomial2 = `Nom scientifique du(des) espèce(s) associée(s)`,
         nom_vernaculaire = `Nom vernaculaire`) %>%
  select(nom_binomial, nom_binomial2, nom_vernaculaire)
exceptions

# on récupère la liste des poissons selon Wikipedia (pas besoin de repasser par les étapes au-dessus)

nom_poissons <- readxl::read_xlsx("data/liste_poissons.xlsx") %>%
  mutate(nom_binomial = `Nom binomial`,
         nom_vernaculaire = `Nom vulgaire`,
         auteur = `Auteur`) %>%
  select(nom_binomial, nom_vernaculaire, auteur)
nom_poissons

# on fusionne deux fichiers
poissons <- bind_rows(nom_poissons, exceptions)
poissons

# on lit donnees ADNe

ADNe <- readxl::read_xlsx("data/ADNe/ADNe_poissons.xlsx", 
                          na = "*")

ADNe_cleanedup <- ADNe %>% 
  pivot_longer(cols = !nom_sp, 
               names_to = "replicas", values_to = "values") %>%
  add_column(type = rep(rep(c("nb_rep_positifs_sur_12", 
                              "nb_seq_adn"), 24), 
                        length(unlist(unique(ADNe[,1]))))) %>%
  filter(type == "nb_seq_adn") %>%
  mutate(sites = case_when(
    str_detect(replicas, "lez_amont") ~ "lez_amont",
    str_detect(replicas, "tann") ~ "tannerie",
    str_detect(replicas, "laval") ~ "lavalette",
    str_detect(replicas, "clin") ~ "cdp",
    str_detect(replicas, "hdr") ~ "hdr",
    str_detect(replicas, "lez_aval_2_ecluse") ~ "lez_aval_2_ecluse",
    str_detect(replicas, "lez_aval_mosson") ~ "lez_aval_mosson",
    str_detect(replicas, "lez_aval_palavas") ~ "lez_aval_palavas",
    str_detect(replicas, "lirou") ~ "lirou",
    str_detect(replicas, "lironde") ~ "lironde")) %>%
  group_by(nom_sp, sites) %>%
  slice_max(values, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-replicas, -type) %>%
  pivot_wider(names_from = sites, values_from = values, values_fill = 0) %>%
  left_join(poissons, by = c("nom_sp" = "nom_binomial")) %>%
  mutate(nom_espece = if_else(!is.na(nom_binomial2), 
                              paste0(nom_sp, " (", nom_binomial2, ")"),
                              nom_sp)) %>%
  select(nom_espece,
         nom_vernaculaire,
         auteur,
         lez_amont, 
         lirou, 
         tannerie,
         lironde,
         lavalette,
         cdp,
         hdr,
         lez_aval_2_ecluse,
         lez_aval_mosson,
         lez_aval_palavas)

ADNe_cleanedup

dates <- c("21/11/2023", # Lez Amont 3 rep	
           "05/12/2023", # Tannerie 3 rep
           "18/12/2023", # Lavalette 3 rep
           "18/12/2023", # Clinique du Parc 3 rep		
           "20/12/2023", # Hôtel de Région 3 rep		
           "22/11/2023", # Lez aval (2ème écluse), Lez aval (confluence), Lez aval (Palavas)
           "21/12/2023", # Lirou 3 rep
           "22/12/2023") # Lironde 3 rep

# colourer <- scales::col_quantile(
#   na.color = "transparent",
#   palette = "Blues",
#   n = 6,
#   domain = c(min(ADNe_cleanedup[,-c(1:3)], na.rm = TRUE), 
#              max(ADNe_cleanedup[,-c(1:3)], na.rm = TRUE)))
# 
# set_flextable_defaults(
#   font.family = "Open Sans", 
#   font.color = "black")
# 
# sect_properties <- prop_section(
#   page_size = page_size(
#     orient = "landscape",
#     width = 500, height = 200
#   ),
#   type = "continuous",
#   page_margins = page_mar()
# )
# 
# ADNe_cleanedup %>%
#   rename("Nom scientifique" = "nom_espece",
#          "Nom vernaculaire" = "nom_vernaculaire",
#          "Auteur" = "auteur") %>%
# flextable() %>%
#   bg(
#     bg = colourer,
#     j = c("lez_amont", 
#           "lirou", 
#           "tannerie",
#           "lironde",
#           "lavalette",
#           "cdp",
#           "hdr",
#           "lez_aval_2_ecluse",
#           "lez_aval_mosson",
#           "lez_aval_palavas"),
#     part = "body") %>%
#   width(j = 1, width = 22) %>%
#   width(j = 2, width = 12) %>%
#   width(j = 3, width = 9) %>%
#   width(j = 4:13, width = rep(6,10)) %>%
#   save_as_docx(path = "outputs/adne.docx",
#                align = "center",
#                pr_section = sect_properties)


# on passe en 1/0 les résultats ADNe

ADNe_cleanedup_binary <- ADNe_cleanedup
mask <- ADNe_cleanedup_binary[,-c(1:3)] > 0
ADNe_cleanedup_binary[,-c(1:3)][mask] <- 1
ADNe_cleanedup_binary
mask <- is.na(ADNe_cleanedup_binary[,-c(1:3)])
ADNe_cleanedup_binary[,-c(1:3)][mask] <- 0
ADNe_cleanedup_binary

# on lit donnees regime alim

regime <- readxl::read_xlsx("data/régime-alimentaire/poisson_ADN_clean.xlsx")
regime

# on les passe en binaire
mask <- is.na(regime[,-c(1)])
regime[,-c(1)][mask] <- 0
regime

# on ajoute des colonnes de NA pour les sites non-echantillonnes

regime_complet <- regime %>% 
  add_column("lironde" = NA, .after = "tannerie") %>%
  add_column("lez_aval_mosson" = NA, .after = "lez_aval_2_ecluse") %>%
  add_column("lez_aval_palavas" = NA, .after = "lez_aval_mosson")

# on ajoute nom vernaculaire et auteur

regime_cleanedup <- regime_complet %>% 
  left_join(poissons, by = c("nom_espece" = "nom_binomial")) %>%
  mutate(nom_espece = if_else(!is.na(nom_binomial2), 
                              paste0(nom_espece, " (", nom_binomial2, ")"),
                              nom_espece)) %>%
  select(nom_espece,
         nom_vernaculaire,
         auteur,
         lez_amont, 
         lirou, 
         tannerie,
         lironde,
         lavalette,
         cdp,
         hdr,
         lez_aval_2_ecluse,
         lez_aval_mosson,
         lez_aval_palavas)

regime <- regime_cleanedup %>%
  add_column("méthode" = "régime alimentaire (ADN)") %>%
  pivot_longer(cols = lez_amont:lez_aval_palavas, 
               names_to = "site", 
               values_to = "espèce_présente")

adne <- ADNe_cleanedup_binary %>%
  add_column("méthode" = "présence-absence (ADNe)") %>%
  pivot_longer(cols = lez_amont:lez_aval_palavas, 
               names_to = "site", 
               values_to = "espèce_présente")

# on remplit les sites non-ech par une methode ou l'autre avec des NAs
df <- rbind(regime, adne) %>%
  complete(`méthode`, nom_espece, site) %>% 
  select(nom_espece, nom_vernaculaire, auteur, méthode, site, espèce_présente) %>%
  arrange(nom_espece) 

df
  
# df_all <- df %>% 
#   pivot_wider(names_from = c(site, méthode), values_from = espèce_présente) %>%
#   select(-nom_vernaculaire, -auteur) %>%
#   select(sort(names(.))) %>%
#   relocate(nom_espece, .before = "cdp_présence-absence (ADNe)")
# 
# df_all <- as.data.frame(df_all)
# df_all[,-1][is.na(df_all[,-1])] <- ""
# tada <- df_all %>%
#   formattable(list(
#     `lez_amont_présence-absence (ADNe)` = color_tile("aquamarine4", "aquamarine"),
#     `lez_amont_régime alimentaire (ADN)` = color_tile("darkorange4", "darkorange"),
#     `hdr_présence-absence (ADNe)` = color_tile("aquamarine4", "aquamarine"),
#     `hdr_régime alimentaire (ADN)` = color_tile("darkorange4", "darkorange"),
#     `lavalette_présence-absence (ADNe)` = color_tile("aquamarine4", "aquamarine"),
#     `lavalette_régime alimentaire (ADN)` = color_tile("darkorange4", "darkorange"),
#     `lez_aval_2_ecluse_présence-absence (ADNe)` = color_tile("aquamarine4", "aquamarine"),
#     `lez_aval_2_ecluse_régime alimentaire (ADN)` = color_tile("darkorange4", "darkorange"),
#     `lez_aval_mosson_présence-absence (ADNe)` = color_tile("aquamarine4", "aquamarine"),
#     `lez_aval_mosson_régime alimentaire (ADN)` = color_tile("darkorange4", "darkorange"),
#     `lez_aval_palavas_présence-absence (ADNe)` = color_tile("aquamarine4", "aquamarine"),
#     `lez_aval_palavas_régime alimentaire (ADN)` = color_tile("darkorange4", "darkorange"),
#     `lironde_présence-absence (ADNe)` = color_tile("aquamarine4", "aquamarine"),
#     `lironde_régime alimentaire (ADN)` = color_tile("darkorange4", "darkorange"),
#     `lirou_présence-absence (ADNe)`	= color_tile("aquamarine4", "aquamarine"),
#     `lirou_régime alimentaire (ADN)`	= color_tile("darkorange4", "darkorange"),
#     `tannerie_présence-absence (ADNe)`	= color_tile("aquamarine4", "aquamarine"),
#     `tannerie_régime alimentaire (ADN)`= color_tile("darkorange4", "darkorange"),
#     `cdp_présence-absence (ADNe)` = color_tile("aquamarine4", "aquamarine"),
#     `cdp_régime alimentaire (ADN)` = color_tile("darkorange4", "darkorange")),
#     align = c("l", "c", "c","c", "c","c", "c","c", "c","c", "c","c", "c","c", "c","c", "c","c", "c","c", "c"))
# 
# library("htmltools")
# library("webshot")    
# 
# export_formattable <- function(f, file, width = "100%", height = "250%", 
#                                background = "white", delay = 0.2)
# {
#   w <- as.htmlwidget(f, width = width, height = height)
#   path <- html_print(w, background = background, viewer = NULL)
#   url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
#   webshot(url,
#           file = file,
#           selector = ".formattable_widget",
#           delay = delay)
# }
# 
# export_formattable(tada,"outputs/FT.png")
# 
# # l'export fail miserably

# on reprend avec flextable


# on va exporter tout ça dans un tableau

df_all <- df %>% 
  select(-nom_vernaculaire, -auteur) %>%
  pivot_wider(names_from = c(site, méthode), values_from = espèce_présente) %>%
  select(sort(names(.))) %>%
  relocate(nom_espece, .before = "cdp_présence-absence (ADNe)")

df_all

columns <- c("lez_amont_présence-absence (ADNe)",
             "lez_amont_régime alimentaire (ADN)",
             "lirou_présence-absence (ADNe)",
             "lirou_régime alimentaire (ADN)",
             "tannerie_présence-absence (ADNe)",
             "tannerie_régime alimentaire (ADN)",
             "lironde_présence-absence (ADNe)",
             "lironde_régime alimentaire (ADN)",
             "lavalette_présence-absence (ADNe)",
             "lavalette_régime alimentaire (ADN)",
             "cdp_présence-absence (ADNe)",
             "cdp_régime alimentaire (ADN)",
             "hdr_présence-absence (ADNe)",
             "hdr_régime alimentaire (ADN)",
             "lez_aval_2_ecluse_présence-absence (ADNe)",
             "lez_aval_2_ecluse_régime alimentaire (ADN)",
             "lez_aval_mosson_présence-absence (ADNe)",
             "lez_aval_mosson_régime alimentaire (ADN)",
             "lez_aval_palavas_présence-absence (ADNe)",
             "lez_aval_palavas_régime alimentaire (ADN)")

# List of columns to loop through
col_regime <- columns[seq(2,20,2)]
col_presence <- columns[seq(1,19,2)]

# Define colors and ranges
bg_picker_presence <- scales::col_factor(
  palette = c("aquamarine4", "aquamarine"),
  domain = c(0, 1),
  na.color = "white") 

bg_picker_regime <- scales::col_factor(
  palette = c("darkorange4", "darkorange"),
  domain = c(0, 1),
  na.color = "white") 

set_flextable_defaults(
  font.family = "Open Sans", 
  font.color = "black")

sect_properties <- prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 500, height = 200
  ),
  type = "continuous",
  page_margins = page_mar()
)


df_all %>%
  select(nom_espece, 
         "lez_amont_présence-absence (ADNe)",
         "lez_amont_régime alimentaire (ADN)",
         "lirou_présence-absence (ADNe)",
         "lirou_régime alimentaire (ADN)",
         "tannerie_présence-absence (ADNe)",
         "tannerie_régime alimentaire (ADN)",
         "lironde_présence-absence (ADNe)",
         "lironde_régime alimentaire (ADN)",
         "lavalette_présence-absence (ADNe)",
         "lavalette_régime alimentaire (ADN)",
         "cdp_présence-absence (ADNe)",
         "cdp_régime alimentaire (ADN)",
         "hdr_présence-absence (ADNe)",
         "hdr_régime alimentaire (ADN)",
         "lez_aval_2_ecluse_présence-absence (ADNe)",
         "lez_aval_2_ecluse_régime alimentaire (ADN)",
         "lez_aval_mosson_présence-absence (ADNe)",
         "lez_aval_mosson_régime alimentaire (ADN)",
         "lez_aval_palavas_présence-absence (ADNe)",
         "lez_aval_palavas_régime alimentaire (ADN)") %>%
  flextable() %>%
#  delete_part(part = "header") %>%
  add_header_row(top = FALSE,
                 values = c("",
                            "Lez amont", 
                            "Lirou", 
                            "Tannerie",
                            "Lironde",
                            "Lavalette", 
                            "Clinique du Parc", 
                            "Hôtel de Région", 
                            "Lez aval - 2ème écluse", 
                            "Lez aval - confluence Mosson", 
                            "Lez aval - Palavas"), 
                 colwidths = c(1, rep(2, 10))) %>%
  align(
    i = 1,
    j = 2:21,
    align = "center",
    part = "all") %>%
  add_header_row(top = FALSE,
                 values = c("",
                            "présence", 
                            "régime", 
                            "présence", 
                            "régime", 
                            "présence", 
                            "régime", 
                            "présence", 
                            "régime", 
                            "présence", 
                            "régime", 
                            "présence", 
                            "régime", 
                            "présence", 
                            "régime", 
                            "présence", 
                            "régime", 
                            "présence", 
                            "régime", 
                            "présence", 
                            "régime"), 
                 colwidths = c(1, rep(1, 20))) %>%
  bg(j = col_presence, bg = bg_picker_presence) %>%
  bg(j = col_regime, bg = bg_picker_regime) %>%
  width(j = 1, width = 22) %>%
  width(j = 2:21, width = rep(6,20)) %>%
  save_as_docx(path = "outputs/adn_adne.docx",
               align = "center",
               pr_section = sect_properties)

# 
# adne <- df %>% 
#   pivot_wider(names_from = site, values_from = espèce_présente) %>%
#   filter(méthode == "présence-absence (ADNe)") %>%
#   select(-nom_vernaculaire, -auteur, -méthode) %>%
#   select(nom_espece,lez_amont, 
#                   lirou, 
#                   tannerie,
#                   lironde,
#                   lavalette,
#                   cdp,
#                   hdr,
#                   lez_aval_2_ecluse,
#                   lez_aval_mosson,
#                   lez_aval_palavas)
# adne
# dim(adne)
# 
# regim <- df %>% 
#   pivot_wider(names_from = site, values_from = espèce_présente) %>%
#   filter(méthode == "régime alimentaire (ADN)") %>%
#   select(-nom_vernaculaire, -auteur, -méthode) %>%
#   select(nom_espece,lez_amont, 
#          lirou, 
#          tannerie,
#          lironde,
#          lavalette,
#          cdp,
#          hdr,
#          lez_aval_2_ecluse,
#          lez_aval_mosson,
#          lez_aval_palavas)
# regim
# dim(regim)
# 
# 
# 
