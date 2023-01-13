################################################################################
# Le but de la fonction est de pouvoir ouvrir et traiter plusieurs fichiers    #
################################################################################
graphics.off()
rm(list=ls())

gf<-function(){graphics.off()}

library(ggplot2)
library(ncdf4)
library(fields)
library(dplyr)
library(tidyr)
library(gridExtra)
source("scripts/fonction_conversion_coordonnee.R")
source("scripts/fonction_carte.R")
source("scripts/fonction_vous_etes_ici.R")



# POLCOM-ERSEM
# Janvier 1985
################################################################################
####### Exemple avec données minimales #########################################
################################################################################
dirpolcom<-"data/"
folder_polcom = list.files(dirpolcom)

list_ncf <- list()

for(i in seq_along(folder_polcom)){
  
}

list("cat", "rabbit")

ncf<-nc_open(paste0(dirpolcom,"CERES_outputs_IFREMER_Aug22.rcp85.2020.0.7.nc"))

nm<-attributes(ncf$var)$names
nm

lsimu<-lapply(nm,ncvar_get,nc=ncf)
names(lsimu)<-nm

lapply(lsimu,dim)

# dealing with missing values (above 9.9...e+36 )--> NAs
missvalf<-function(x){
  x[x>1E36]<-NA 
  return(x)
}
lsimu<-lapply(lsimu,missvalf)
################################################################################
################################################################################