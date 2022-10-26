

graphics.off()
rm(list=ls())

gf<-function(){graphics.off()}

library(ggplot2)
library(ncdf4)
library(fields)


# POLCOM-ERSEM
# Janvier 1985

dirpolcom<-"data/"
list.files(dirpolcom)

ncf<-nc_open(paste0(dirpolcom,"CERES_outputs_IFREMER_Aug22.rcp85.2020.07.nc"))

nm<-attributes(ncf$var)$names
nm

lsimu<-lapply(nm,ncvar_get,nc=ncf)
names(lsimu)

lapply(lsimu,dim)

# dealing with missing values (above 9.9...e+36 )--> NAs
missvalf<-function(x){
  x[x>1E36]<-NA 
  return(x)
}
lsimu<-lapply(lsimu,missvalf)



#-----------------------------
# variables

## Fonctions cosmétiques qui permettent d'extraire les noms complets des variables et les unités.  

var_unit<-function(x){return(ncf$var[[x]]$units)}
var_lname<-function(x){return(ncf$var[[x]]$longname)}
vard<-cbind.data.frame(nm=nm,longname=sapply(nm,var_lname),units=sapply(nm,var_unit),
                       minval=unlist(lapply(lsimu,min,na.rm=T)),maxval=unlist(lapply(lsimu,max,na.rm=T)))
rownames(vard)<-NULL
vard

#-----------------------------
# coordinates
## variable utile pour avoir les cartes complètes Pas utiles autrement
lat<-apply(lsimu$latbnd,2,mean)
lon<-apply(lsimu$lonbnd,2,mean)
?apply()
range(lat) # X
# [1] 43.0 58.9
range(lon) # Y
# [1] -0.5  8.9

#-----------------------------
# Depth
range(lsimu$depth,na.rm=T)
# [1] -2426.9699707    -0.1263089
head(lsimu$depth)
# bottom depth
bd<-apply(lsimu$depth,1:2,min)
bd2<-bd
bd[1,3]
table(bd>-200) # borner les profondeurs pour voir les petites profondeurs
bd2[bd<(-200)]<-(-200)

table(phyto1 > 2000)
zop <- phyto1
zop[phyto1>(2000)] <- 2000

phyto1 <- apply(lsimu$Z4c, c(1:2),sum)
truc<- sapply(phyto1, max, na.rm = T)
max(truc[!is.infinite(truc)])
length(truc)

dev.new()
image.plot(bd2,x=lon,y=lat,main='Depth (m)', xlab = "Longitude", ylab= "Latitude" )
?image.plot
dev.off()
dim(bd2)
#-----------------------------
# Temperature
range(lsimu$ETW,na.rm=T)
ncf$var$ETW$units
par(mfrow=c(1,1),xpd=TRUE)
# SST
dev.new()
###########################################################################
# Bon plot pour le vous etes ici
#imagePlot(lsimu$P1c[,,1],x=lon,y=lat,main='SST (°C)')
#points(x = -9 , y = 47, pch = "*", cex=1.5, col="dark red")
###########################################################################
xline(lon)
yline(lat)
dev.off()
grid(nx=210, ny=160)

lsimu$parea

# SBT
grid()

max<-  max(lsimu$ETW[55,55,])
min<-  min(lsimu$ETW[55,55,])


par(mfrow=c(1,1))
#ggplot(data = as.data.frame(lsimu$ETW[55,55,]))
plot <- qplot(x=lsimu$ETW[55,55,] , y=(1:40), xlab = "temperature", ylab = "profondeur") +
              scale_x_continuous(name = "temperature", limits = c(min,max)) + # Pour voir la variation il faut que les bornes soient cohérentes
              scale_y_reverse() + # Pour voir la thermocline avec la profondeur déscendante
#              geom_smooth(method = "gam") # L'equation qui fit les points pas necessaire mais bon.
plot


#-----------------------------
# Biomasses (concentrations /m3 or /m2)
par(mfrow=c(2,2))

# Benthos
benthos<-lsimu$Y2c+lsimu$Y3c+lsimu$Y4c

dim(lsimu$Y2c)
image.plot(benthos,x=lon,y=lat,main='Benthos (mgC/m2)')

# cell height
# cellh<-abs(lsimu$zbnd[1,,,]-lsimu$zbnd[2,,,])

# Phytoplankton
phytopm3<-lsimu$P1c+lsimu$P2c+lsimu$P3c+lsimu$P4c
phytop<-apply(phytopm3*lsimu$pdepth,1:2,sum,na.rm=T) # biomass integrated over the whole water column
image.plot(phytop,x=lon,y=lat,main='Phytoplankton (mgC/m2)')

# Zooplankton
zoopm3<-lsimu$Z4c+lsimu$Z5c+lsimu$Z6c
zoop<-apply(zoopm3*lsimu$pdepth,1:2,sum)# biomass integrated over the whole water column
image.plot(zoop,x=lon,y=lat,main='Zooplankton (mgC/m2)')
