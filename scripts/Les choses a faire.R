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


lsimu$P1c[55,55,]
sum(which(lsimu$depth[55,55,] < -100))
?sum

lsimu$depth[55,55,][31]

# Permet de trouver quels valeurs satisfont une condition (indice)
vect_dep<- which(lsimu$depth[55,55,] < -100)
lsimu$depth[55,55,][31] # indice de la valeur voulue
sum(lsimu$depth[55,55,][vect_dep])


temp <- which(lsimu$ETW[,,] > 17)
sum(lsimu$ETW[,,][temp])
str(lsimu$ETW)
dim(lsimu$ETW[,,])[1]

sum_vect <- vector()

for( i in 1:dim(lsimu$ETW)[1]){
  for (j in 1:dim(lsimu$ETW)[2]){
    som <- sum(lsimu$ETW[i,j,], na.rm = F)
    sum_vect <- append(sum_vect, som)
  }
}
?append
plot(sum_vect)

lsimu$ETW[55,55,4]

ma <- matrix(c(1:24), ncol = 3, nrow = 8)

ma
apply(ma, 1, table)  #--> a list of length 2
apply(ma, 1, stats::quantile) # 5 

température <- lsimu$ETW[50:55, 50:55, ]
température
mapply(sum(x), température[,,x], dim(température)[3])

température[1,1,]
sum(température[1,1,])
apply(température, c(1:2),sum) # Ca fonctionne 
?apply


sum(température[6,6,])

integration_verticale <- function(simulation, parameter, above, below, area )
  
cell_depth <- lsimu$pdepth
cell_depth[62,28,]
  
dim(lsimu$pdepth)


sum(lsimu$pdepth[121,121,][which(lsimu$depth[121,121,] > -100)])
lol <- map_position(lsimu, longitude = -0, latitude = 55)
conversion_coordonnee(lsimu, longitude = -0, latitude = 55)

profond<- lsimu$pdepth[55,55,]

index <- which(lsimu$depth[55,55,] > -100)
which(lsimu$depth[55,55,]> -100)



sum(lsimu$pdepth[55,55,][index]) -100
lsimu$pdepth[55,55,][length(index)-1]
profond[28]
profond[length(profond)]
sum(profond)
lsimu$depth[55,55,] > -100
# select the interval that's interesting then apply.


# protocole dans l'ordre
lsimu$pdepth[55,55,] # la taille des cases a une coordonnées 
interval <- lsimu$depth[,1:100,] # La valeur a chaque point, représentant la moyenne de la case 
sum(lsimu$pdepth[55,55,]) # Véritable profondeur pour un intervalle donné
gc(reset = T)
st<- Sys.time()
test <- apply(interval, MARGIN = c(1:2),function(x)which(x> -100)) # indices des profondeurs au dessus d'un seuil
end <- Sys.time()
end - st


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
    switch (depth,
      surface = depth <- 1,
      bottom = depth <- 40,
    )
    return(map_profile(simulation =  simulation, parameter = parameter, depth = depth, main = main))
  }
  if(is.numeric(depth)){ 
    depth <- abs(depth)
    simu_depth <- abs(simulation[["depth"]])
    max_depth <- max(abs(simulation[["depth"]]), na.rm = T)
    if( depth > max_depth) return(c("Out of bounds, please enter depth below :", round(max_depth,2)))
    cube_size <- simulation[["pdepth"]]
    # return(simu_depth)
    vecs_to_integrate <- apply(simu_depth, MARGIN = c(1:2) , function(x)which(x <  depth))# Les vecteur qui remplissent la condition de taille pour l'intégration. 
    return(vecs_to_integrate)
    }
  }

    
c <- map_profile_integration(simulation =  lsimu, parameter = "pdepth", depth = 8,
                             above = T, main = "carte B1c")
c

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
