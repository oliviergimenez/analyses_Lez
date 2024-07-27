library(readxl) # charge le package qui va bien

# lit le fichier de donnees
dat <- read_xlsx("C:/Users/tomca/OneDrive/Bureau/Stage_CEFE/Analyse_sans_rep.xlsx", col_types = "text")  
dat <- read_xlsx("Analyse_sans_rep.xlsx", col_types = "text")  
dat$Heure <- as.numeric(dat$Heure) - 1

# convertit l'heure classique e radial
dat$radtime<-as.numeric(dat$Heure)*2*pi
#extrait une espèce

# cherche ou sont les renards
mask <- dat$`Espèce / Activité` == "Renard"

# extrait heure radiale pour renard
renardtime <- dat$radtime[mask]

# on se debarrasse des NA qui sont temporaires
renardtime <- renardtime[!is.na(renardtime)]

# charge package pour calcul des overlap
library(overlap)

# activ pour renard
densityPlot(renardtime)

# cherche ou sont les renards
mask <- dat$`Espèce / Activité` == "Humain/Chien"

# extrait heure radiale pour hum chien
humchtime <- dat$radtime[mask]

# on se debarrasse des NA qui sont temporaires
humchtime <- humchtime[!is.na(humchtime)]

# activ pour hum ch
# Olivier : juste la figure
densityPlot(humchtime,
            xlab = "Temps", 
            ylab = "Densité", 
            main = "Activité humain/chien",
            col = "brown",
            lwd = 2)

# Olivier : la figure sauvegardée en png avec une bonne résolution
png(filename = "activ_humch.png",
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)
par(
  mar      = c(5, 5, 2, 2),
  xaxs     = "i",
  yaxs     = "i",
  cex.axis = 2,
  cex.lab  = 2,
  cex.main = 2
)
densityPlot(humchtime,
            xlab = "Temps", 
            ylab = "Densité", 
            main = "Activité humain/chien",
            col = "brown",
            lwd = 2)
dev.off()


# chevauchement renard hum/chien

# Olivier : juste la figure
overlapPlot(renardtime,humchtime, 
            xlab = "Temps", 
            ylab = "Densité", 
            main = "Chevauchement de l'activité renard et humain/chien",
            linet = c(1,1),
            linew = c(2,2),
            linec = c("orange", "brown"))
legend("topleft", 
       legend = c("renard", "humain/chien"), 
       cex = 1,
       lty = 1, 
       lwd = 2,
       col = c("orange", "brown"), 
       bg = "white")


# Olivier : la figure sauvegardée en png avec une bonne résolution
png(filename = "overlap_renard_humch.png",
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
overlapPlot(renardtime,humchtime, 
            xlab = "Temps", 
            ylab = "Densité", 
            main = "Chevauchement de l'activité renard et humain/chien",
            linet = c(1,1),
            linew = c(2,2),
            linec = c("orange", "brown"))
legend("topleft", 
       legend = c("renard", "humain/chien"), 
       cex = 1.5,
       lty = 1, 
       lwd = 2,
       col = c("orange", "brown"), 
       bg = "white")
dev.off()



# cherche ou sont les sanglier
mask <- dat$`Espèce / Activité` == "Sanglier"

# extrait heure radiale pour sanglier
sangtime <- dat$radtime[mask]

# on se debarrasse des NA qui sont temporaires
sangtime <- sangtime[!is.na(sangtime)]

# activ pour sanglier
densityPlot(sangtime)

# chevauchement
overlapPlot(sangtime,humchtime)

#calculer le pourcentage d'overlap
overlapEst(renardtime,humchtime,type="Dhat1")

#interval de confiance autour de la valeur
pseudorenard <- resample(renardtime, 1000)
pseudohum <- resample(humchtime, 1000)
bootrenardhum <- bootEst(pseudorenard, pseudohum, type = "Dhat1")
quantile(bootrenardhum, probs = c(2.5, 97.5)/100)

# cherche ou sont les loutres
mask <- dat$`Espèce / Activité` == "Loutre"

# extrait heure radiale pour loutre
loutime <- dat$radtime[mask]

# activ pour loutre
densityPlot(loutime)

# chevauchement
overlapPlot(loutime,humchtime)

# cherche ou sont les fouines
mask <- dat$`Espèce / Activité` == "Fouine"

# extrait heure radiale pour fouine
fouitime <- dat$radtime[mask]

# activ pour fouine
densityPlot(fouitime)

# chevauchement
overlapPlot(fouitime,humchtime)
