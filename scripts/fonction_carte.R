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
library(ggmap)
library(maps)
library(mapdata)
library(sf)
library(tidyverse)
library(raster)
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

map_profile <- function(simulation, parameter,layer = 0,  main = parameter){
  if(layer < 1) return("Please enter the layer (positive)")
  dimsim <- length(dim(simulation[[parameter]]))
  if (dimsim == 3) {measure <- simulation[[parameter]][,,layer]}
  if (dimsim == 2) {measure <- simulation[[parameter]][,]}
    # Limits of the map. 
  lat<-apply(simulation[["latbnd"]],2,mean)
  lon<-apply(simulation[["lonbnd"]],2,mean)
    #plot
  plot <- image.plot(measure, x=lon, y=lat, main = main)
  return(measure)
}


map_profile(simulation =  lsimu, parameter = "B1c", layer = 1, main = "carte B1c")
dim(c)
c[55,55,]

max(lsimu$B1c, na.rm = T)



map_profile_integration <- function(simulation, parameter,depth = c('surface','bottom', numeric()), above = T,  main = "Titre"){
  
  # test the depth parameter
  if(is.character(depth)){
  depth <- match.arg(depth) 
  print(depth)
  } else if (is.numeric(depth))return(depth)
  else return("depth : invalid format")
  
  # if(depth < 1) return("Please enter the depth (positive)")
  # measure <- simulation[[parameter]][,,depth]
  # 
  # apply(measure, c(1:2),sum)
  # 
  # # Limits of the map. 
  # lat<-apply(lsimu$latbnd,2,mean)
  # lon<-apply(lsimu$lonbnd,2,mean)
  # #plot
  # plot <- image.plot(measure, x=lon, y=lat, main = main)
  # #  return(plot)
}
map_profile_integration(depth = 1000)
class(-1000)
numeric()
interval <- lsimu$depth[50:55, 50:55,]
apply(interval, MARGIN = c(1:2), conversion_coordonnee(interval))
dep <- simulation[["depth"]][case_lon,case_lat,]
case_dep <- which.min(abs(dep - depth))


dev.new()
map_profile_integration(simulation =  lsimu, parameter = "B1c", depth = 40, main = "carte B1c")








################################################################################
# Select a subregion
# single_county <- subset(ut_county, subregion=="utah")
# 
# # Fill the selected subregion with a predefined color and
# # plot a colored point with a specified long. and lat.
# ut_base + theme_void() +
#   geom_polygon(data = ut_county, fill = NA, color = "white") +
#   geom_polygon(color = "black", fill = NA) +
#   geom_polygon(data = single_county, fill = "red", color = "white") +
#   geom_point(x=-111.8, y=40.2, col="blue", size=3)
# 
# 
# map_data(c(lsimu[[latbnd]], lsimu[[lonbnd] ))
# lon
# lsimu$latbnd
# str(lsimu)
# 
# 
# ################################################################################
# 
# x<- 1:10
# y<- 1:15
# z<- outer( x,y,"+") 
# image.plot(x,y,z) 
# 
# # or 
# obj<- list( x=x,y=y,z=z)
# image.plot(bd2, legend.lab="Sverdrups")
# 
# ################################################################ 
# # the next sequence of examples explain how to quickly 
# # adpat this basic plot to include morre features
# # In another direction see the very last example where 
# # we use many of the setting in base R graphic to mimic a 
# # (beautiful) ggplot version. 
# ###############################################################
# #
# # add some points on diagonal using standard plot function
# #(with some clipping beyond 10 anticipated)
# 
# points( 0.9, 0.4, pch="+", cex=0.75, col = "red")
