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
conversion_coordonnee(lsimu, longitude = -8, latitude = 48)
conversion_coordonnee(lsimu, longitude = -1, latitude = 43)

map_position(lsimu, longitude = -8, latitude = 48)

lsimu$depth[c(42:111),c(51:46),1] == lsimu$depth[c(111:42),c(46:51),1] 
lsimu$depth[c(42:111),c(51:46),1]
lsimu$depth[111, 51,1]
lsimu$lonbnd[1,111]
lsimu$latbnd[1,51:46]




car1 = lsimu$depth[c(42:111), c(51:46),1]
car2 = lsimu$depth[c(62:111), c(46:41),1]
carf = car1 + car2
str(car1)

bob <- lapply(lsimu, function(el){
  borne_lon <- c(42:111)
  borne_lat <- c(51:1)
  if (length(dim(el)) == 3) return(el[borne_lon, borne_lat,]) # P1c:Parea variables indexés lat lon profondeur

  if (length(dim(el)) == 2){ 
    if (dim(el)[1] == 2){ 
      if (dim(el)[2] == 160) {return(el[,borne_lat])} # $latbnd
      if (dim(el)[2] == 210) {return(el[,borne_lon])} # $lonbnd
    }
    else return(el[borne_lon, borne_lat]) # MLD Y(2:4)C variables indexées lat lon
    
  }
  if (length(dim(el)) == 4) return(el[,borne_lon, borne_lat,]) # zbnd
  })

####### Extrait un carré de donné dans une simulation plus grande ########

extract_area <- function(simulation, borne_lon, borne_lat){
  small_map<- lapply(simulation, function(el){
#    borne_lon <- c(42:111)
#    borne_lat <- c(51:1)
    if (length(dim(el)) == 3) return(el[borne_lon, borne_lat,]) # P1c:Parea variables indexés lat lon profondeur
    
    if (length(dim(el)) == 2){ 
      if (dim(el)[1] == 2){ 
        if (dim(el)[2] == 160) {return(el[,borne_lat])} # $latbnd
        if (dim(el)[2] == 210) {return(el[,borne_lon])} # $lonbnd
      }
      else return(el[borne_lon, borne_lat]) # MLD Y(2:4)C variables indexées lat lon
      
    }
    if (length(dim(el)) == 4) return(el[,borne_lon, borne_lat,]) # zbnd
  })
  return(small_map)
  }

##### Fonction pour enlever les petites zones une fois le crré extrait #####
extrude_zone <- function(simulation, borne_lon, borne_lat, val=NA){
  simulation <- lapply(simulation, function(el){
    if (length(dim(el)) == 3){ 
      el[borne_lon, borne_lat,] <- val
      return(el)} # P1c:Parea variables indexés lat lon profondeur
    
    if (length(dim(el)) == 2){ 
      if (dim(el)[1] == 2){ 
        if (dim(el)[2] == 160) { # On garde les données de positionnement mais on supprime les valeurs des variables
          #el[,borne_lat]<-  val
          return(el)} # $latbnd
        if (dim(el)[2] == 210) {
          #el[,borne_lon]<-  val
          return(el)} # $lonbnd
      }
      else {
      el[borne_lon, borne_lat] <-val 
      return(el) # MLD Y(2:4)C variables indexées lat lon
      }
    }
    if (length(dim(el)) == 4) {
     # el[,borne_lon, borne_lat,]<-val
      return(el) # zbnd
    }
  })
  return(simulation)
  }

em <- extrude_zone(lsimu, borne_lon = c(42:111), borne_lat =  c(51:1))
em$latbnd[,51]

truc <- lsimu
truc$depth[, 42:43, 13:40]


# Fonction pour vider les valeurs 
# 
# 