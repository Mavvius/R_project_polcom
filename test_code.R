## Test de fonction et de trucs  ##

### Faire le truc de Jerome sur le chargement des libraires necessaires ###

library(ggplot2)
library(ncdf4)
library(fields)
library(tidyr)
library(readr)
library(stringr)
library(dplyr)

###########################################################################

# Test de la fonction paste0 Pour creer plusieurs variable en applicant une fonction #

# var_names <- paste0("p", 1:10)
# values <- seq(100, 1200, by = 100)
# 
# for (i in seq_along(var_names)) assign(var_names[[i]], values[[i]])
# 
# mget(var_names)
# p1

## On peut assigner autant de variables que necessaires pour les differents fichiers ##
# On pourrait avoir des variables differenci?es par la date de la mesure ##

## Aller chercher les donnees ##
data <- "data/"   # Ne fonctionne que si on est en chemin relatif
polcom_files <- list.files(path = data, pattern = "*.nc", all.files = T)
polcom_files
##########################################


length(polcom_files)
### Creer une variable par observation ###
## A faire différencier les ficher par la date ##

# liste des differentes variables #
ncf_names <- paste0("ncf", 1:length(polcom_files))
nm_names <- paste0("nm", 1:length(polcom_files))
lsimu_names <- paste0("lsimu", 1:length(polcom_files))

attributes(ncf1$var)$names
## Fonction pour borner les valeurs ##
missvalf<-function(x){
  x[x>1E36]<-NA 
  return(x)
}
######################################

### Creer une variable par simulation ### 
#Remarque : ralentissement au moment lsimu 

for (i in seq_along(ncf_names)){
  assign(ncf_names[[i]], nc_open(paste0(data,polcom_files[i]))) # Assigne ncf[i] on ouvre le ncdf et on lui donne le bon nom
  temp <- assign(ncf_names[[i]], nc_open(paste0(data,polcom_files[i]))) # Assigne ncf[i] on ouvre le ncdf et on lui donne le bon nom
  assign(nm_names[[i]], attributes(temp$var)$names) # On extrait les attributs # 
  tnm <- assign(nm_names[[i]], attributes(temp$var)$names)
  assign(lsimu_names[[i]], lapply(tnm,ncvar_get,nc=temp))
}


