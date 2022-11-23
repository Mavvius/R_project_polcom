##########################
# Les choses a faire, 
# - "intégration verticale." faire la somme des valeurs pondérés sur la colonne d'eau
#   Cad ponderer par la hauteur de la case. (Deux options 1 si les valeurs sont egales tout sommer
#    option 2 si les valeurs sont inégales ponderer sur la hauteur de case.)
# - Fonction de conversion de profondeur. parametre profondeur (en m). en cases "bottom" 40 "surface" 1
# - Pour la somme etape 1 -> trouver la case la plus proche
#                 etape 2 -> trouver la distance la plus proche et ponderer le morceau pris sur la vraie taille de la case. 
#- donner la possiblité d'exxtraire la donnée, brut ou transformée plutot qu'un plot
##################



# Permet de trouver quels valeurs satisfont une condition (indice)

température <- lsimu$ETW[50:55, 50:55, ]
température
mapply(sum(x), température[,,x], dim(température)[3])

température[1,1,]
sum(température[1,1,])
apply(température, c(1:2),sum) # Ca fonctionne 

sum(température[6,6,])

integration_verticale <- function(simulation, parameter, above, below, area )
  
cell_depth <- lsimu$pdepth
cell_depth[62,28,]
  

sum(lsimu$pdepth[65,65,][which(lsimu$depth[65,65,] > -95)])
lol <- map_position(lsimu, longitude = -0, latitude = 55)
conversion_coordonnee(lsimu, longitude = -0, latitude = 55)

profond<- lsimu$pdepth[55,55,]

index <- which(lsimu$depth[55,55,] > -100)
which(lsimu$depth[55,55,]> -100)

class(lsimu$depth)
?array

sum(lsimu$pdepth[55,55,][index]) -100
lsimu$pdepth[55,55,][length(index)-1]
profond[28]
profond[length(profond)]
sum(profond)
lsimu$depth[55,55,] > -100
# select the interval that's interesting then apply.


# protocole dans l'ordre
lsimu$pdepth[55,55,] # la taille des cases a une coordonnées 
sum(lsimu$pdepth[55,55,]) # Véritable profondeur pour un intervalle donné
gc(reset = T)
which(interval > -100)
st<- Sys.time()
test[55,55]
end <- Sys.time()
end - st



smin[55,55,][unlist(smte[55,55])]

smic[55,55,][1:length((unlist(smte[55,55])))]


newar<- array(numeric(), c(55,55,1))

sum(smin[55,55,][1:(length(unlist(smte[55,55]))+1)])
smin[55,55,][40] > 100
length(unlist(smte[55,55]))
1:(length(unlist(smte[55,55]))+1) # Permet d'acceder a la case d'après ou d'avant -1
newar <- array(numeric(), c(dim(smin)[1],dim(smin)[2],1))
newar <- apply(newar,  c(1:2), )
count <- 0
count_petits <- 0 



# smic[i,j,][1:(length(unlist(smte[i,j]))-1)] Le liste des trucs a multiplier. 

?weighted.sum
sum(x * w, na.rm = T) # Some pondéré. On pourra faire la somme des valeurs du var avec la taille des cases comme ça. 



dim(newar)
summary(smic)
larg <- list(c(1:55))
long <- list(c(1:55))
deuxD <- list(larg, long)
?list
dim(interval)[1]

class(test[65,65])
interval[65,65,]

length(lsimu$pdepth[65,65,][unlist(test[65,65])])

interval <- interval[,,][unlist(test)][55]
dim(test)
image.plot(interval)


class(c(1:29))

var(lsimu$pdepth[55,55,]) # verification que la taille des cases est uniques. 
max(abs(lsimu$depth), na.rm =T)

#     else if(depth == "bottom") {
#       depth <- 40 
#       return(map_profile(simulation =  simulation, parameter = parameter, depth = 40, main = main)
#     }
#     if (is.numeric(depth) & abs(depth) < max_depth){}
#     else{ return("Wrong format or Out of bound. Depth must be under maximum depth")} # Poser la question pour le not 0
#   return(depth)
# }
 #   if(depth < 1) return("Please enter the depth (positive)")#
 # 
 #  
 #    measure <- simulation[[parameter]][,,depth] 
 # 
 #   apply(measure, c(1:2),sum)
 # 
 #   # Limits of the map.
 #   lat<-apply(lsimu$latbnd,2,mean)
 #   lon<-apply(lsimu$lonbnd,2,mean)
 #   
 #   plot <- image.plot(measure, x=lon, y=lat, main = main)
 #   return(plot)
 # }

map_profile_integration <- function(simulation, parameter,depth = c('surface','bottom', numeric()), 
                                    above = T ,  main = "Titre"){
  # test the depth parameter ## Fait comme ca pour l'instant. Mais trouver un meilleur moyen
  if(is.character(depth)){
    depth <- match.arg(depth)
    ### Si on demande la couche de surface ou de profondeur on prend la 1ère ou 
    ###la dernière case. A mon avis pertinent pour le fond mais pas pour la surface.
    ### Parcontre genere des erreurs selon la taille des cellules prises. s
    switch (depth, 
      surface = depth <- 1,
      bottom = depth <- 40, # Si on définit autrement la couche de mélange on verra comment faire.  
    )
    return(map_profile(simulation =  simulation, parameter = parameter, depth = depth, main = main))
  }
  if(is.numeric(depth)){ 
    depth <- abs(depth)  # Profondeur demandée
    ##1. Valeur de profondeur du modèle
    simu_depth <- abs(simulation[["depth"]]) 

    max_depth <- max(abs(simulation[["depth"]]), na.rm = T) # Profondeur max pour determiner les limites. 

    if( depth > max_depth) return(c("Out of bounds, please enter depth below :", round(max_depth,2)))

    # return(simu_depth)
    ##2. Les vecteur qui remplissent le prérequis de profondeur.
    index_vect_under_threshold <- apply(simu_depth, MARGIN = c(1:2) , function(x)which(x < depth)) 
    
    ##3. La hauteur des cellules du modèle pour chaque coordonnée
    # Hauteur de la grille si max_depth < 150 égale sinon augmente non linéaire
    cube_size <- simulation[["pdepth"]] 
    
    ##4. Les valeurs pour le paramètre d'interet
    simu_val <- simulation[[parameter]]
    
    cumul_depth<- array(numeric(), c(dim(simu_depth)[1],dim(simu_depth)[2],1))
    map_val_output<- array(numeric(), c(dim(simu_depth)[1],dim(simu_depth)[2]))
    
    ##### Le test de seuil pour l'integration verticale. #####
    for (i in 1:dim(simu_depth)[1]) { # Longitude
      for (j in 1:dim(simu_depth)[2]) { # Latitude
        #print(j)
        last_index <- length(unlist(index_vect_under_threshold[i,j]))
        ##################################################### jusqu'ici tout va bien 
        
        if (last_index != 0 ) { # Eviter les NA 
          # Somme des cases le plus proches du seuil
          # Vectors of cube size for matching depth values
          water_col <- cube_size[i,j,][1:last_index]
          cumul_depth[i,j,] <- sum(water_col)
          
          # Si case en trop ou derniere case trop grande
          if (cumul_depth[i,j,] > depth) {
            dif <- cumul_depth[i,j,] - depth
            lich <- cube_size[i,j,][last_index] - dif
         #   cumul_depth[i,j,] <- sum(cube_size[i,j,][1:(last_index)-1)]) +lich
            map_val_output[i,j] <- sum(simu_val[i,j,][1:(last_index-1)] * cube_size[i,j,][1:(last_index-1)]) + (lich * simu_val[i,j,][(last_index)])    
          }
          # S'il manque une case
          else if ((cumul_depth[i,j,] < depth) & (simu_depth[i,j,][40] > depth)){ 
            dif <- abs(cumul_depth[i,j,] - depth) # la quantité manquante
            lich <- cube_size[i,j,][last_index+1] - dif # la case en plus
            cumul_depth[i,j,] <- cumul_depth[i,j,] +dif 
            map_val_output[i,j] <- sum(simu_val[i,j,][1:(last_index-1)] * cube_size[i,j,][1:(last_index-1)]) + (lich * simu_val[i,j,][(last_index)])
            if(is.infinite(map_val_output[i,j])) return(1)
          }else map_val_output[i,j] <- sum(simu_val[i,j,])        
        } 
      } 
    }
    
    # Limits of the map. 
    lat<-apply(simulation[["latbnd"]],2,mean)
    lon<-apply(simulation[["lonbnd"]],2,mean)
    #return(map_val_output)
    plot <- image.plot(map_val_output, x=lon, y=lat, main = main)
      
    return(map_val_output)
    }
  }

c <- map_profile_integration(simulation =  lsimu, parameter = "B1c", depth = 20, above = T, main = "carte B1c")
summary(c)
lsimu$B1c[96,41,]
lsimu$depth[96,41,]

lsimu$B1c[11,121,]
lsimu$depth[11,121,]

c[101,71]
sum(lsimu$depth[1,1,])
length(lsimu$depth[1,1,])


image.plot(c)
dim(lsimu$P1c[,,][,,40])
?image.plot
interval <- abs(lsimu$depth) # La valeur a chaque point, représentant la moyenne de la case 
intercase <- lsimu$pdepth
test <- apply(interval, MARGIN = c(1:2),function(x)which(x < 100)) # indices des profondeurs au dessus d'un seuil
phyto <- lsimu$P1c[1:55,1:55,]

sum(phyto[1:29] *smic[55,55,][1:29])
newar <- array(numeric(), c(dim(smin)[1],dim(smin)[2],1))

smin <- interval[1:55,1:55,] # valeurs de profondeur
smte <- test[1:55,1:55] # indices qui remplissent la condition < 100 
smic <- intercase[1:55,1:55,] # Pdepth

##### Le test de seuil pour l'integration verticale. #####
for (i in 1:dim(smin)[1]) { # Longitude
  for (j in 1:dim(smin)[2]) { # Latitude
    vec_size <- length(unlist(smte[i,j]))
    
    if (vec_size != 0 ) { # Eviter les NA 
      newar[i,j,] <- sum(smic[i,j,][unlist(smte[i,j])]) # Somme des cases le plus proches du seuil
      
      if (newar[i,j,] > 100) { # Si case en trop ou derniere case trop grande
        dif <- newar[i,j,] -100
        lich <- smic[i,j,][vec_size] - dif
  #      print(lsimu$pdepth[i,j,][unlist(smte[i,j])])
        newar[i,j,] <- sum(smic[i,j,][1:(vec_size-1)]) +lich }
      
      else if ((newar[i,j,] < 100) & (smic[i,j,][40] > 100)){ # S'il manque une case
       # count <- count +1
        dif <- abs(newar[i,j,] - 100) # la quantité manquante
        #print(c("d :",dif))
        lich <- smic[i,j,][vec_size+1] - dif # la case en plus
        print(c("L :",lich))
        newar[i,j,] <- newar[i,j,] +dif }
      
    } 
  } 
}

plot(phyto)

truc <- 1:(length(unlist(smte[55,55]))+1)













p <- geom_freqpoly(mean(lsimu$pdepth, na.rm =T), )
p

t<- c('surface','bottom', as.numeric(seq(from = 1, to = 5, by = 1 )))
class(t[5])
ibjet <- "65a"
class(as.numeric(ibjet))



c <- map_profile_integration(simulation =  lsimu, parameter = "pdepth", depth = "surface", above = T, main = "carte B1c")
c
min(lsimu$depth, na.rm = T)

lsimu$latbnd[1,]
lsimu$lonbnd[1,]
  # Trouver les indices 
  # si la derniere case + 1/2 x la taille de la case < 100 il faut ajouter une case
  # Verfier dans le cas improbable ou la case proche de 99 ne dépasse pas 100
  # 
  # 
  # 
  # 
  # 
  # 
## Pas besoin de s'embeter a verifier avec précision que cumsum est bon 
## puisque, c'est le milieu de la case si la suivante est superieur a 100 
##

### Correct  way to write function  
#   fooBar <- function(x,y){
#     if(missing(y)) {
#       x
#     } else {
#       x + y
#     }
#   }
# 
# fooBar(3,1.5)
# # [1] 4.5
# fooBar(3)
# # [1] 3  
