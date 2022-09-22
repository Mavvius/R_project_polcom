## Test de fonction et de trucs  ##

# Test de la fonction paste0 Pour creer plusieurs variable en applicant une fonction #

var_names <- paste0("p", 1:10)
values <- seq(100, 1200, by = 100)

for (i in seq_along(var_names)) assign(var_names[[i]], values[[i]])

mget(var_names)
p1

## On peut assigner autant de variables que necessaires pour les différents fichiers ##
# On pourrait avoir des variables différenciées par la date de la mesure ##
polcom_path <- "C:/Users/aelassim/Desktop/projet_alternance_polcom/data/"
polcom_files <- list.files(path = "C:/Users/aelassim/Desktop/projet_alternance_polcom/data/", pattern = "*.nc", all.files = T)

length(polcom_files)

ncf_names <- paste0("ncf", 1:length(polcom_files))
nm_names <- paste0("nm", 1:length(polcom_files))
lsimu_names <- paste0("lsimu", 1:length(polcom_files))
for (i in seq_along(ncf_names)){
  assign(ncf_names[[i]], nc_open(paste0(polcom_path, polcom_files[[i]]))) # Assigne ncf[i] on ouvre le ncdf et on lui donne le bon nom
  assign(nm_names[[i]], attributes(ncf_names[[i]]$var)$names) # On extrait les attributs
  assign(lsimu_names[[i]], lapply(nm_names[[i]],ncvar_get,nc=ncf_names[[i]]))
}


nm<-attributes(ncf$var)$names # attributs du tableau
nm

lsimu<-lapply(nm,ncvar_get,nc=ncf)
names(lsimu)<-nm