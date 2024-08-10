library(tidyverse)
library(readxl)
library(lubridate)

dat <- read_xlsx("C:/Users/tomca/Documents/Pièges photo/pieges_photo_classes.xlsx") # lit le fichier de donnees 

dat_sans_repet <- NULL
ind <- 1

for (i in 1:nrow(dat)){
  
  if ((ind == nrow(dat)) || (ind == (nrow(dat) + 1))){
    dat_sans_repet <- rbind(dat_sans_repet, dat[ind,])
    break
  }
  
  sp_courant <- dat$`Espèce / Activité`[ind]
  sp_suivant <- dat$`Espèce / Activité`[ind+1]
  tps_courant <- as.numeric(dat$Heure[ind])
  tps_suivant <- as.numeric(dat$Heure[ind+1])
  
  if (sp_courant != sp_suivant){
    dat_sans_repet <- rbind(dat_sans_repet, dat[ind,])
    ind <- ind + 1
  }
  
  if ((sp_courant == sp_suivant) & (tps_courant - tps_suivant) < 0.1){
    dat_sans_repet <- rbind(dat_sans_repet, dat[ind+1,])
    ind <- ind + 2
  }
  
  if ((sp_courant == sp_suivant) & (tps_courant - tps_suivant) > 0.1){
    dat_sans_repet <- rbind(dat_sans_repet, dat[ind,])
    ind <- ind + 1
  }
}
write.csv(dat_sans_repet,
          file="C:/Users/tomca/Documents/Pièges photo/Analyse_sans_rep.csv",
          row.names = FALSE,
          col.names = NA)
library(openxlsx)
write.xlsx(dat_sans_repet,
           file="C:/Users/tomca/Documents/Pièges photo/Analyse_sans_rep.xlsx")
