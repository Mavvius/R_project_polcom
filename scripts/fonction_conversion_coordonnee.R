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
lsimu$latbnd[55]
lsimu$lonbnd[55]

coor <- c(lsimu$latbnd,lsimu$lonbnd)
coor[1:10]
range(lsimu$latbnd)
  # Find the nearest value of something
  # https://stackoverflow.com/questions/43472234/fastest-way-to-find-nearest-value-in-vector
which.min(abs(lsimu$latbnd - 47.72))

lsimu$latbnd[96]


dim(lsimu$lonbnd)
lsimu$depth
conversion_coordonnee <- function(simulation, latitude, longitude, depth){
  lat <- simulation[["latbnd"]]
  lon <- simulation[["lonbnd"]]
  case_lat <- which.min(abs(lat - latitude))
  case_lon <- which.min(abs(lon - longitude))
  return(c(lat[case_lat], lon[case_lon]))
  return(c(case_lat, case_lon))
}
conversion_coordonnee(lsimu, latitude = 43.21, longitude = -1)
