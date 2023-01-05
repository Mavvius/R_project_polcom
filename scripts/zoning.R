################################################################################
# Le but de la fonction est de faire des zones opérationnelles pour travailler 
#plus simplement
################################################################################
graphics.off()
rm(list=ls())

gf<-function(){graphics.off()}

library(ggplot2)
library(ncdf4)
library(fields)
library(dplyr)
library(plyr)
library(tidyr)
library(ggmap)
library(maps)
library(mapdata)
library(sf)
library(tidyverse)
library(raster)
source("scripts/fonction_conversion_coordonnee.R")
source("scripts/fonction_vous_etes_ici.R")

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

conv <- conversion_coordonnee(lsimu, longitude = -6, latitude = 48)
conversion_coordonnee(lsimu, longitude = -2, latitude = 44.5)
conversion_coordonnee(lsimu, longitude = -1, latitude = 43)

map_position(lsimu, longitude = -8, latitude = 48)

lsimu$depth[c(c(42:111),c(62:111)), c(c(51:46),c(46:41)),1]
lsimu$depth[111, 51,1]
lsimu$lonbnd[1,111]
lsimu$latbnd[1,51:46]

carr1 = c(c(-8,48),c(-1,47.5)) # 42 51 ; 111 46
carr2 = c(c(-6,47.5),c(-1,47)) # 62 46 ; 111 41 
carr3 = c(c(-5,47),c(-1,46)) # 72 41 ; 111 31
carr4 = c(c(-4,46),c(-1,45.5)) # 82 31 ; 111 26
carr5 = c(c(-3,45.5),c(-1,44.5)) # 91 26 ; 111 16
carr6 = c(c(-2,44.5),c(-1,43)) # 101 16 ; 111 1


car1 = lsimu$depth[c(42:111), c(51:46),1]
car2 = lsimu$depth[c(62:111), c(46:41),1]
carf = car1 + car2
str(car1)

dim(lsimu)
lapply(lsimu, function(el) if (length(dim(el)) == 3) el[1:12, 4:50, ] else NA)

# Fonction pour delimiter la zone. 

reduce_map <- function(el_in_simu, borne_lon, borne_lat, val){
    if (length(dim(el_in_simu)) == 3) el_in_simu[borne_lon[1]:borne_lon[2], borne_lat[1]:borne_lat[2],]
    if (length(dim(el_in_simu)) == 2) el_in_simu[borne_lon[1]:borne_lon[2], borne_lat[1]:borne_lat[2]]
    if (length(dim(el_in_simu)) == 4) el_in_simu[borne_lon[1]:borne_lon[2], borne_lat[1]:borne_lat[2]]
}

dim(lsimu$zbnd)

# Fonction pour vider les valeurs 
# 
# reduce_map <- function(simulation, borne_lon, borne_lat, val){
#   
# }