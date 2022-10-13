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
conversion_coordonnee <- function(simulation, longitude, latitude, depth){
    #Extract the values 
  lat <- simulation[["latbnd"]][1,]
  lon <- simulation[["lonbnd"]][1,]
    # Check if the values are within boundaries of the table
  if(!between(latitude, min(lat), max(lat)) | !between(longitude, min(lon), max(lon))){
    return("Please enter longitude values between [-12.05 ; 8.95] and latitude values between [42.95 ; 58.95] ")
   }else{
     # Find the nearest value rounded up for both coordinates
   case_lat <- which.min(abs(lat - latitude))
   case_lon <- which.min(abs(lon - longitude))
   #return(c(lat[case_lat], lon[case_lon]))
   return(c(case_lon, case_lat))
  }
}

conversion_coordonnee(lsimu, longitude = -8, latitude = 47)

lsimu$ETW[1,1,]
lsimu$lonbnd


?between
################################################################################
 # conversion_coordonnee(lsimu, latitude = 4.65, longitude = -5)

# lsimu$latbnd[55]

# test_fun <- function( latitude, longitude, sim, org) {
#   if(between(latitude, 9.3, 14 ) | between(longitude, 25, 42)){
#     return("nop")
#   }else{
#     return("yup")
#   }
# }
# 
# 
# test_fun(latitude = 12, longitude=28)
# 
# conversion_coordonnee(lsimu, latitude = 64, longitude = -14)
# 
# between(64, 42.9, 59)
# range(lsimu$latbnd)# 42.95 58.95
# range(lsimu$lonbnd)# -12.05 8.95
# 
# coor <- c(lsimu$latbnd,lsimu$lonbnd)
# coor[1:10]
# range(lsimu$latbnd)
# # Find the nearest value of something
# # https://stackoverflow.com/questions/43472234/fastest-way-to-find-nearest-value-in-vector
# which.min(abs(lsimu$latbnd - 47.72))
# 
# lsimu$latbnd[96]
# 
# min(lsimu$lonbnd)
# 
# lsimu$depth
