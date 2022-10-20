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
