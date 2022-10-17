################################################################################
# Le but de la fonction est de convertir les données géographique en données
# en cases sur la carte. 
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
map_position <- function(simulation, longitude, latitude, main = "* You are here"){
    # Limits of the map. 
    lat<-apply(simulation[["latbnd"]],2,mean)
    lon<-apply(simulation[["lonbnd"]],2,mean)
    #plot
    par(mfrow=c(1,1))
    measure <- simulation[["P1c"]][,,1]
    plot <- image(measure, x=lon, y=lat, main = main, col = "blue")
    #plot <- image.plot(measure, x=lon, y=lat, main = main, legend.cex = 0)
    points(x = longitude , y = latitude, pch = "*", cex=1.5, col="dark red")
    return(plot)
  }
lol <- map_position(lsimu, longitude = -5, latitude = 50)
lol
