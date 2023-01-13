################################################################################
#Les zones délimités                                                           #
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
source("scripts/zoning.R")

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
BoB <- function(simulation){
  
  simulation <- extrude_zone(simulation, borne_lon = 42:62, borne_lat =  1:46)
  simulation <- extrude_zone(simulation, borne_lon = 62:72, borne_lat =  1:41)
  simulation <- extrude_zone(simulation, borne_lon = 72:82, borne_lat =  1:31)
  simulation <- extrude_zone(simulation, borne_lon = 82:91, borne_lat =  1:26)
  simulation <- extrude_zone(simulation, borne_lon = 91:101, borne_lat = 1:16)
  
  simulation <- extract_area(simulation, borne_lon = c(42:111), borne_lat =  c(1:51))
  
  
  return(simulation)
}

lsimu$depth[62:72, 41:1,1]

map_position(test, longitude = -6, latitude = 46)


test <- lsimu
test <- extract_area(test, borne_lon = c(42:111), borne_lat =  c(51:1))
test <- extrude_zone(test, borne_lon = 42:62, borne_lat =  46:1)
test <- extrude_zone(test, borne_lon = 62:72, borne_lat =  41:1)

conversion_coordonnee(bayBiscay, longitude = -6, latitude =46 )

bayBiscay <- BoB(lsimu)
bayBiscay$depth

map_profile(simulation =  bayBiscay, parameter = "B1c", layer = 1)
vertical_profile(bayBiscay, parameter = "ETW" , longitude = -2.5, latitude = 46)





tbob <- lsimu[["Y3c"]][,]
labob <- apply(lsimu[["latbnd"]],2, mean)
lonbob <- apply(lsimu[["lonbnd"]],2, mean) 
image.plot(tbob, x=lonbob, y=labob, main="truc")
dim(lsimu$Y2c)


tbob <- bayBiscay[["depth"]][,,1]
labob <- apply(bayBiscay$latbnd,2, mean)
lonbob <- apply(bayBiscay$lonbnd,2, mean) 
image.plot(tbob, x=labob, y=lonbob, main="titre")
conversion_coordonnee(simulation = lsimu, longitude = -3, latitude = 44.5)
conversion_coordonnee(simulation = lsimu, longitude = -2, latitude = 43)

grdcar <- c(42:111, 51:1) # Longitude puis latitude du grand carré
# Extrudage ensuite
car1 <-  c(42:62, 46:1)
car2 <-  c(62:72, 41:1)
car3 <- c(72:82, 31:1)
car4 <- c(82:91, 26:1)
car5 <- c(91:101, 16:1)
  