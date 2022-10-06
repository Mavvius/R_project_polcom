################################################################################
# Le but de la fonction est de pouvoir visualiser des cartes de paramètres. 
# en entrant le paramètre et les coordonnées.
################################################################################
graphics.off()
rm(list=ls())

gf<-function(){graphics.off()}

library(ggplot2)
library(ncdf4)
library(fields)
library(dplyr)
library(tidyr)
source("scripts/fonction_conversion_coordonnee.R")
# POLCOM-ERSEM
# Janvier 1985
################################################################################
####### Exemple avec données minimales #########################################
################################################################################
dirpolcom<-"data/"
list.files(dirpolcom)

ncf<-nc_open(paste0(dirpolcom,"CERES_outputs_IFREMER_Aug22.rcp85.2020.07.nc"))

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

map_profile <- function(simulation, parameter,depth = 0,  main = "Titre"){
  measure <- simulation[[parameter]][,,depth]
    # Limits of the map. 
  lat<-apply(lsimu$latbnd,2,mean)
  lon<-apply(lsimu$lonbnd,2,mean)
    #plot
  par(mfrow=c(1,1))
  plot <- image.plot(measure, x=lon, y=lat, main = main)
#  return(plot)
}

map_profile(simulation =  lsimu, parameter = "P1c", depth = 40,main = "surface_T")

