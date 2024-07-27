library(readxl) # charge le package qui va bien

dat <- read_xlsx("/Users/oliviergimenez/Dropbox/OG/BOULOT/ENCADREMENT/Tom CARLI/analyses/final/Analyse_sans_rep.xlsx", col_types = "text") # lit le fichier de donnees 
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
densityPlot(humchtime)

# chevauchement
overlapPlot(renardtime,humchtime)

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
