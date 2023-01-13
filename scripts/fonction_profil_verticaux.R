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
library(gridExtra)
source("scripts/fonction_conversion_coordonnee.R")
source("scripts/fonction_carte.R")
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
################################################################################

vertical_profile <- function(simulation, parameter, longitude, latitude, xlab = parameter, ylab = "Depth", position = F){
  
    # Convert geographic coordinates into indices and checks that it's in bounds
  coor_convertie <- conversion_coordonnee(simulation,longitude = longitude, latitude = latitude)
  if (is.character(coor_convertie)) return(coor_convertie)

  measure <- simulation[[parameter]][coor_convertie[1],coor_convertie[2],]

      # Check if there are values measured for the parameters
  if (NA %in% measure){
    map_position(simulation = simulation,longitude, latitude)
    return("You are out of the boundaries of the model, please try other coordinates")
  }
   # Boundaries of the plot
  max<- max(measure)
  min<- min(measure)  
     # Extract the depth point
  depth_points <- simulation[["depth"]][coor_convertie[1],coor_convertie[2],]
  
  # if (position = T){
  #    #par(mfrow= c(2,1))
  # #  position <- map_position(simulation = simulation,longitude, latitude)}
  #   mark <- map_position(simulation = simulation,longitude, latitude)
  # }
    plot <- qplot(x=measure, y=depth_points, xlab = xlab, ylab = ylab) + # Pour voir la thermocline avec la profondeur déscendante en prenant les vraies valeurs pas besoin d'inverser la courbe
    scale_x_continuous(name = xlab, limits = c(min,max))+  # Pour voir la variation il faut que les bornes soient cohérentes
     geom_line() # L'equation qui fit les points pas necessaire mais bon.
  #ob <- grid.arrange(plot, mark, ncol = 2)
  return(plot)
}


vertical_profile(lsimu, "ETW", longitude = -5 , latitude = 46.65, xlab = "Température", position = T)

#vertical_profile(lsimu, "ETW", longitude =43.6 , latitude = -4, xlab = "Temperatour")


################################################################################

#########################Cimetiere des bouts de codes ##########################

# vertical_profile_ori <- function(simulation, parameter, longitude, latitude, xlab = parameter, ylab = "Depth" ){
#   # coor_convertie <- conversion_coordonnee(simulation,longitude, latitude)
#   # return(coor_convertie[1])
#   measure <- simulation[[parameter]][latitude,longitude,]
#   max<- max(measure)
#   min<- min(measure)  
#   par(mfrow=c(1,1))
#   plot <- qplot(x=measure, y=simulation$depth[latitude,longitude,], xlab = xlab, ylab = ylab) + # Pour voir la thermocline avec la profondeur déscendante en prenant les vraies valeurs pas besoin d'inverser la courbe
#     scale_x_continuous(name = xlab, limits = c(min,max))+  # Pour voir la variation il faut que les bornes soient cohérentes
#     
#     geom_line() # L'equation qui fit les points pas necessaire mais bon.
#   return(plot)
# }
# vertical_profile_ori(lsimu, "ETW", longitude =55 , latitude = 55, xlab = "Temperatour")

