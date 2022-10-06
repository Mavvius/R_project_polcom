################################################################################
# Le but de la fonction est de pouvoir visualiser des profils verticaux
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

vertical_profile <- function(simulation, parameter, longitude, latitude, xlab = parameter, ylab = "Depth" ){
  measure <- simulation[[parameter]][longitude,latitude,]
  max<- max(measure)
  min<- min(measure)  
  par(mfrow=c(1,1))
  plot <- qplot(x=measure, y=simulation$depth[longitude,latitude,], xlab = xlab, ylab = ylab) + # Pour voir la thermocline avec la profondeur déscendante en prenant les vraies valeurs pas besoin d'inverser la courbe
    scale_x_continuous(name = xlab, limits = c(min,max))+  # Pour voir la variation il faut que les bornes soient cohérentes
                                      
     geom_line() # L'equation qui fit les points pas necessaire mais bon.
  return(plot)
}


vertical_profile(lsimu, "ETW", longitude = 55, latitude = 55, xlab = "Temperatour")

lsimu$depth[120,120,]
lsimu$pdepth[120,120,]


lsimu$ETW[120,120,]

################################################################################

