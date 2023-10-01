###############################################
# Script R Session 2-  Les bases du Langage R

###############################################
######## Plot Graphs and Maps ################################
# Graphics- Package maps 
library(maps)
MM <- map("world")
MM
attributes(MM) # description de l�objet
length(MM$names) # nombre total de polygones (pays et iles) group�s dans l�objet MM
MM$names #afficher les noms de polygones (pays et iles)
MM$names[1]	#afficher le premier nom de polygone
AA <- map("world","Aruba")
AA

## ecrire la suite
#Ajuster la fen�tre graphique, ajouter les coordonn�es, la l�gende 

# ouvrir une fen�tre graphique en sp�cifiant les dimensions
windows(15, 10)

# fixer la taille des marges (bas/gauche/haut/droite)
par(mar=c(4.5,4.5,2,1))

# Repr�senter les axes (lon/lat)
plot(c(-180,180),c(-90,90),xlab="longitude", ylab="latitude", type="n", main = "4 pays�Europe")
# Ajouter le fond de carte 'monde'
map("world", fill = TRUE, col = NULL, add=T)
# On ajoute un couche avec la France en rouge, puis Italie en orange.....
map("world", region="France", fill = TRUE, col = 2, add=T)
map("world", region=�Italy�, fill = TRUE, col = �orange�, add=T)
map("world", region= c("Spain","Portugal"), fill = TRUE, exact=T,col = c("blue","purple"), add=T)

# ajouter une legende
legend(x=-170, y = -30 , legend=c("France","Italy","Spain","Portugal"), exact=T, fill = c("red","orange","purple","blue"), border = "black") 

# ligne horizonatale/verticale
abline(h=0, col="grey")
abline(v=0, col="grey")



