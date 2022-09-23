

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

range(lat) # X
# [1] 43.0 58.9
range(lon) # Y
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
par(mfrow=c(1,2),xpd=TRUE)
# SST

image.plot(lsimu$ETW[,,1],x=lon,y=lat,main='SST (?C)')
grid(nx=210, ny=160)

# SBT
grid()

par(mfrow=c(1,1))
#ggplot(data = as.data.frame(lsimu$ETW[55,55,]))
plot <- qplot(x=lsimu$ETW[55,55,] , y=(1:40), xlab = "temperature", ylab = "profondeur") +
              scale_x_continuous(name = "temperature", limits = c(min,max)) +
              scale_y_reverse()
plot
qplot()

max<-  max(lsimu$ETW[55,55,])
min<-  min(lsimu$ETW[55,55,])


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
