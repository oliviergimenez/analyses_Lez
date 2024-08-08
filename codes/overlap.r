library(tidyverse)
theme_set(theme_light(base_size = 15))
library(readxl)
library(overlap) # calcul des overlap
library(rphylopic)

#---- 0. nombre de contacts par espèce

# lecture et formatage des données
dat <- read_xlsx("data/pieges_photo_classes.xlsx", col_types = "text")  

# nb total de contacts
nrow(dat)

# par espèce (au moins 20 contacts)
dat %>% 
  filter(`Espèce / Activité` != "NA") %>%
  count(`Espèce / Activité`, sort = TRUE) %>%
  filter(n >= 20) %>%
  print(n = Inf)
  
#---- 1. overlap loutres / humains

# lecture et formatage des données
dat <- read_xlsx("data/camtrap_sans_rep.xlsx", col_types = "text")  
dat$Heure <- as.numeric(dat$Heure) - 1
dat$radtime <- as.numeric(dat$Heure)*2*pi # heure en radial

# obs de loutres
mask <- dat$`Espèce / Activité` == "Loutre"
# extrait heure radiale
loutre <- dat$radtime[mask]

# obs d'humains avec ou sans chien
mask <- dat$`Espèce / Activité` == "Humain/Chien"
# extrait heure radiale 
hum <- dat$radtime[mask]

# icone pour loutre 
uuid <- get_uuid(name = "Lutra lutra")
lutra <- get_phylopic(uuid = uuid)

# icone pour humain
#pick_phylopic(name = "Homo sapiens")
homo <- get_phylopic(uuid = "c089caae-43ef-4e4e-bf26-973dd4cb65c5")

# la figure sauvegardée en png avec une bonne résolution
png(filename = "outputs/overlap_loutre_hum.png",
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)
par(
  mar      = c(5, 5, 2, 2),
  xaxs     = "i",
  yaxs     = "i",
  cex.axis = 1.5,
  cex.lab  = 1.5,
  cex.main = 1.5
)
overlapPlot(loutre, hum, 
            xlab = "Temps", 
            ylab = "Densité", 
            main = "",
            linet = c(1,1),
            linew = c(2,2),
            linec = c("orange", "brown"),
            adjust = 2)
add_phylopic_base(img = lutra, color = "orange", x = 20.5, y = 0.17, ysize = .02)
add_phylopic_base(img = homo, color = "brown", x = 22.5, y = 0.17, ysize = .03)
dev.off()

# pourcentage d'overlap
min(length(loutre), length(hum))
overlapEst(loutre, hum, type = "Dhat1") # 0.04983341 

# intervalle de confiance
pseudosp1 <- resample(loutre, 1000)
pseudosp2 <- resample(hum, 1000)
boot <- bootEst(pseudosp1, pseudosp2, type = "Dhat1")
quantile(boot, probs = c(2.5, 97.5)/100) #  0.03054865 0.11077455

#---- 2. overlap loutres / renards

# obs de loutres
mask <- dat$`Espèce / Activité` == "Loutre"
# extrait heure radiale
loutre <- dat$radtime[mask]

# obs de renards
mask <- dat$`Espèce / Activité` == "Renard"
# extrait heure radiale 
renard <- dat$radtime[mask]

# icone pour loutre 
uuid <- get_uuid(name = "Lutra lutra")
lutra <- get_phylopic(uuid = uuid)

# icone pour renard
#pick_phylopic(name = "Vulpes vulpes")
vulpes <- get_phylopic(uuid = "9b3e3567-2238-40bf-ab5d-bcb6ce7b32d5")

# la figure sauvegardée en png avec une bonne résolution
png(filename = "outputs/overlap_loutre_renard.png",
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)
par(
  mar      = c(5, 5, 2, 2),
  xaxs     = "i",
  yaxs     = "i",
  cex.axis = 1.5,
  cex.lab  = 1.5,
  cex.main = 1.5
)
overlapPlot(loutre,renard, 
            xlab = "Temps", 
            ylab = "Densité", 
            main = "",
            linet = c(1,1),
            linew = c(2,2),
            linec = c("orange", "brown"),
            adjust = 2)
add_phylopic_base(img = lutra, color = "orange", x = 18, y = 0.14, ysize = .015)
add_phylopic_base(img = vulpes, color = "brown", x = 21.5, y = 0.14, ysize = .013)
dev.off()

# pourcentage d'overlap
min(length(loutre), length(renard))
overlapEst(loutre, renard, type = "Dhat1") # 0.5998867

# intervalle de confiance
pseudosp1 <- resample(loutre, 1000)
pseudosp2 <- resample(renard, 1000)
boot <- bootEst(pseudosp1, pseudosp2, type = "Dhat1")
quantile(boot, probs = c(2.5, 97.5)/100) #  0.4500897 0.7779854

#---- 3. overlap loutres / ragondins

# obs de loutres
mask <- dat$`Espèce / Activité` == "Loutre"
# extrait heure radiale
loutre <- dat$radtime[mask]

# obs de ragondins
mask <- dat$`Espèce / Activité` == "Ragondin"
# extrait heure radiale 
rag <- dat$radtime[mask]

# icone pour loutre 
uuid <- get_uuid(name = "Lutra lutra")
lutra <- get_phylopic(uuid = uuid)

# icone pour ragondin
#pick_phylopic(name = "Myocastor coypus")
nutria <- get_phylopic(uuid = "5ee3b11f-9cd8-4aaf-a59e-ac1ddc18d5e8")

# la figure sauvegardée en png avec une bonne résolution
png(filename = "outputs/overlap_loutre_rag.png",
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)
par(
  mar      = c(5, 5, 2, 2),
  xaxs     = "i",
  yaxs     = "i",
  cex.axis = 1.5,
  cex.lab  = 1.5,
  cex.main = 1.5
)
overlapPlot(loutre,rag, 
            xlab = "Temps", 
            ylab = "Densité", 
            main = "",
            linet = c(1,1),
            linew = c(2,2),
            linec = c("orange", "brown"),
            adjust = 2)
add_phylopic_base(img = lutra, color = "orange", x = 18.5, y = 0.14, ysize = .015)
add_phylopic_base(img = nutria, color = "brown", x = 22, y = 0.14, ysize = .015)
dev.off()

# pourcentage d'overlap
min(length(loutre), length(rag))
overlapEst(loutre, rag, type = "Dhat1") # 0.7896944

# intervalle de confiance
pseudosp1 <- resample(loutre, 1000)
pseudosp2 <- resample(rag, 1000)
boot <- bootEst(pseudosp1, pseudosp2, type = "Dhat1")
quantile(boot, probs = c(2.5, 97.5)/100) # 0.6637268 0.9017093


#---- 4. overlap loutres / sangliers

# obs de loutres
mask <- dat$`Espèce / Activité` == "Loutre"
# extrait heure radiale
loutre <- dat$radtime[mask]

# obs d'humains avec ou sans chien
mask <- dat$`Espèce / Activité` == "Sanglier"
# extrait heure radiale 
sangliers <- dat$radtime[mask]

# icone pour loutre 
uuid <- get_uuid(name = "Lutra lutra")
lutra <- get_phylopic(uuid = uuid)

# icone pour humain
#pick_phylopic(name = "Sus scrofa")
scrofa <- get_phylopic(uuid = "3d8acaf6-4355-491e-8e86-4a411b53b98b")

# la figure sauvegardée en png avec une bonne résolution
png(filename = "outputs/overlap_loutre_sangliers.png",
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)
par(
  mar      = c(5, 5, 2, 2),
  xaxs     = "i",
  yaxs     = "i",
  cex.axis = 1.5,
  cex.lab  = 1.5,
  cex.main = 1.5
)
overlapPlot(loutre,sangliers, 
            xlab = "Temps", 
            ylab = "Densité", 
            main = "",
            linet = c(1,1),
            linew = c(2,2),
            linec = c("orange", "brown"),
            adjust = 2)
add_phylopic_base(img = lutra, color = "orange", x = 18, y = 0.14, ysize = .02)
add_phylopic_base(img = scrofa, color = "brown", x = 21, y = 0.14, ysize = .02)
dev.off()

# pourcentage d'overlap
min(length(loutre), length(sangliers))
overlapEst(loutre, sangliers, type = "Dhat1") # 0.5863325

# intervalle de confiance
pseudosp1 <- resample(loutre, 1000)
pseudosp2 <- resample(sangliers, 1000)
boot <- bootEst(pseudosp1, pseudosp2, type = "Dhat1")
quantile(boot, probs = c(2.5, 97.5)/100) # 0.4737075 0.7980883
