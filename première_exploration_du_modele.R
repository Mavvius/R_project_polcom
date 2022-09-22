
setwd("C:/Users/aelassim/Desktop/Alternance_2022") # Mettre le wd pour pas etre embeté par la suite
graphics.off()
rm(list=ls())

gf<-function(){graphics.off()}

library(ggplot2) # ggplot not installing pour une raison qui m'échappe
library(ncdf4)
library(fields)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(tibble)

# POLCOM-ERSEM
# Janvier 1985

#polcom_files <- list.files(path = "C:/Users/aelassim/Desktop/projet_alternance_polcom/data/", pattern = "*.nc", all.files = T)
#polcom_files

dirpolcom<-getwd()# Pour faire fonctionner la fonction paste0
list.files(dirpolcom)

#ncf<-nc_open("CERES_outputs_IFREMER_Aug22.hist.1985.01.nc")

ncf <- ncf1

nm<-attributes(ncf$var)$names # attributs du tableau
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



#-----------------------------
# variables

var_unit<-function(x){return(ncf$var[[x]]$units)}
var_lname<-function(x){return(ncf$var[[x]]$longname)}
vard<-cbind.data.frame(nm=nm,longname=sapply(nm,var_lname),units=sapply(nm,var_unit),
                       minval=unlist(lapply(lsimu,min,na.rm=T)),maxval=unlist(lapply(lsimu,max,na.rm=T)))
rownames(vard)<-NULL
vard

#-----------------------------
# coordinates
lat<-apply(lsimu$latbnd,2,mean)
lon<-apply(lsimu$lonbnd,2,mean)

range(lat)
# [1] 43.0 58.9
range(lon)
# [1] -0.5  8.9

#-----------------------------
# Depth
range(lsimu$depth,na.rm=T)
# [1] -2426.9699707    -0.1263089

# bottom depth
bd<-apply(lsimu$depth,1:2,min)
bd2<-bd
bd2[bd<(-200)]<-(-200)
image.plot(bd2,x=lon,y=lat,main='Depth (m)')

#-----------------------------
# Temperature
range(lsimu$ETW,na.rm=T)
ncf$var$ETW$units
par(mfrow=c(1,2))
# SST
image.plot(lsimu$ETW[,,1],x=lon,y=lat,main='SST (°C)') # température de surface
# SBT
image.plot(lsimu$ETW[,,40],x=lon,y=lat,main='SBT (°C)') # température de fond

#-----------------------------
# Biomasses (concentrations /m3 or /m2)
par(mfrow=c(2,2))

# Benthos
benthos<-lsimu$Y2c+lsimu$Y3c+lsimu$Y4c
image.plot(benthos,x=lon,y=lat,main='Benthos (mgC/m2)')

# cell height
# cellh<-abs(lsimu$zbnd[1,,,]-lsimu$zbnd[2,,,])

# Phytoplankton
phytopm3<-lsimu$P1c+lsimu$P2c+lsimu$P3c+lsimu$P4c
phytop<-apply(phytopm3*lsimu$pdepth,1:2,sum,na.rm=T) # biomass integrated over the whole water column
image.plot(phytop,x=lon,y=lat,main='Phytoplankton (mgC/m2)')

# Zooplankton
zoopm3<-lsimu$Z4c+lsimu$Z5c+lsimu$Z6c
zoop<-apply(zoopm3*lsimu$pdepth,1:2,sum,na.rm=T)# biomass integrated over the whole water column
image.plot(benthos,x=lon,y=lat,main='Zooplankton (mgC/m2)')

