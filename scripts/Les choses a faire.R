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
which(  < 100)



sum(lsimu$pdepth[55,55,][index]) -100
lsimu$pdepth[55,55,][length(index)-1]
profond[28]
profond[length(profond)]
sum(profond)
lsimu$depth[55,55,] > -100
# select the interval that's interesting then apply.


# protocole dans l'ordre
lsimu$pdepth[55,55,] # la taille des cases a une coordonnées 
lsimu$depth[55,55,] # La valeur a chaque point, représentant la moyenne de la case 
sum(lsimu$pdepth[55,55,]) # Véritable profondeur pour un intervalle donné
which(lsimu$depth[55,55,]> -100) # indices des profondeurs au dessus d'un seuil
var(lsimu$pdepth[55,55,]) # verification que la taille des cases est uniques. 


map_profile_integration <- function(simulation, parameter,depth = c('surface','bottom', numeric()), above = T ,  main = "Titre"){
  
  # test the depth parameter
  #if(depth == "pierre") return("tu vois")
  if(depth == "surface") depth <- 1
    else if(depth == "bottom") depth <- 40
    else if (is.numeric(depth) & abs(depth) < 4600){}
    else return("no")
  return(depth)
}
#   if(depth < 1) return("Please enter the depth (positive)")#   

#   measure <- simulation[[parameter]][,,depth]
# 
#   apply(measure, c(1:2),sum)
# 
#   # Limits of the map.
#   lat<-apply(lsimu$latbnd,2,mean)
#   lon<-apply(lsimu$lonbnd,2,mean)
#   #plot
#   plot <- image.plot(measure, x=lon, y=lat, main = main)
#   #  return(plot)
 # }

 c <- map_profile_integration(simulation =  lsimu, parameter = "pdepth", depth = "0", above = T, main = "carte B1c")
c
min(lsimu$depth, na.rm = T)
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
