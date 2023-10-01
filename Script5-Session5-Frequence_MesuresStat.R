### Script 5 ### Frequence et mesures statistiques

# Lire le fichier de donnees: "Data_INSEE_2015", sep=";", header=T
# Fonction readLines, n=10

readLines("Data_INSEE_2015.txt", n=10)

# Importer les donn�es
# Fonction read.table

# Prendre connaissance du fichier: head, tail, dim

#A. Fr�quence � distribution statistique
# 1- Fr�quence: classes, amplitude, intervalle r�gulier

length(MyData$Esp)

range(MyData$Esp)

# Decrire le graphique obtenu
HistEsp <- hist(MyData$Esp)

# Expliquer les informations produite par la fonction hist
HistEsp 


# Note: d�finir � priori de nombre de classe et l'amplitude r�gulier des intervalles
1+log2(96)
1+log2(length(MyData$Esp))
(max(MyData$Esp) - min (MyData$Esp)) / (1+log2(length(MyData$Esp)))

# Objectif: pour chaque d�partement connaitre la classe dans laquelle il tombe
# Utiliser la fonction cut ()et expliquer ce qu'elle permet de d�finir
Seq1 <- seq(79.5,84,length.out=10)
Seq1

Interv <- cut(MyData$Esp, Seq1,right=F) # 
Interv 

# Pour verif
# Afficher les r�sulats dans un tableau
temp <- data.frame(Dep=MyData$Dep, Esp = MyData$Esp,Interval= as.character(Interv),rank = as.numeric(Interv))
temp 



# 2- Fr�quence: D�finir les effectifs par classe (intervalle)
# utiliser la fonction table()

TabEsp <- table(cut(MyData$Esp, Seq1,right=F))
TabEsp

# Exercice: Donner le nombre de d�partements pour lesquels l�esp�rance de vie moyenne 
# est entre 79.5 et 80.5 ans, 80.5 et 81.5 �intervalle 1




#3- D�finir la fr�quence relative

TabEsp <- table(cut(MyData$Esp, Seq1,right=F))

TabEsp / 96	 #freq relative 	

(TabEsp / 96)*100	# freq relative %

# representation graphique
windows(10,10)
par(mfrow=c(2,3)) 	# 6 graphiques r�partis en 2 rows et 3 col


plot(TabEsp )
plot(TabEsp /96)
plot((TabEsp /96)*100)


plot(TabEsp , type="s")
plot(TabEsp /96, type="s")
plot((TabEsp /96)*100, type="s")



## Exercice: utilisation du package Lattice, fonction histogram()
library(lattice)
windows(10,10)

HEsp <- histogram(MyData$Esp, type="percent", nint=7, endpoints= c(79.5,84), col= 4, 
	xlab="Esperance de vie", ylab="% Dep", 
	main= "Frequence des departements \n en fonction de l'esperance de vie")

print(HEsp, split = c(1, 1, 2, 2), more = TRUE)


## Afficherquatres histogramme sur le mem écran
NV <- histogram(MyData$NivVie, type="percent", nint=8, endpoints= c(13000,21000), col= 3,
    xlab="Niveau de vie ", ylab="% Dep", 
    main= "Frequence des départements \n en fonction du niveau de vie")

print(NV , split = c(1, 2, 2, 2), more = TRUE)

TX <- histogram(MyData$TXCh, type="percent", nint=10, endpoints= c(5,15), col= 2,
    xlab="Taux de Chomage", ylab="% Dep", 
    main= "Frequence des départements \n en fonction du taux de chomage")

print(TX , split = c(2, 1, 2, 2), more = TRUE)

MS <- histogram(MyData$MedSpe, type="percent", nint=11, endpoints= c(50,600), col= 5,
    xlab="Nombre de Medecins specialistes", ylab="% Dep", 
    main= "Frequence des départements \n en fonction du nombre de médecins spécialistes")

print(MS , split = c(2, 2, 2, 2), more = F)



# 4- Fr�quence cumulative croissante
cumsum(HistEsp$counts)

x11()
plot(cumsum(HistEsp$counts), col=2)

x11()
plot(HistEsp$mids,cumsum(HistEsp$counts), col=2)


#Exercice:Dans une meme fenetre graphique par() argument mfrow=c(2,2)



#################
##	B. Mesures statistiques
#5- Position (tendance centrale): moyenne, m�diane

# Effectif
length(MyData$Esp)

# Moyenne, m�diane (position)
mean(MyData$Esp) 
median(MyData$Esp) 

#6- Dispersion: 
#a- �tendue, �cart inter-quartile, boite de dispersion
range(MyData$Esp) 	# etendue

quantile (MyData$Esp, 0.75) 
quantile(MyData$Esp, 0.25)
quantile(MyData$Esp, 0.75) - quantile(MyData$Esp, 0.25)

IQR(MyData$Esp)

#Boite de dispersion
boxplot(MyData$Esp, col=4)
boxplot(MyData$Esp, col=4, horizontal=T)

library(lattice) # package lattice

bwplot(MyData$Esp, horizontal = T, col=2) 

#b- Variance, �cart-type
sum((MyData$Esp- mean(MyData$Esp))^2 / (length(MyData$Esp)-1))

var(MyData$Esp)

sqrt(var(MyData$Esp))
sd(MyData$Esp)


# Exercise- s'entrainer avec les autres variables



###################
#C. test de normalit�
shapiro.test(MyData$Esp)

# pour les autres variables


####################################
# Maps,exemple
library(maps)

seuil <- HistEsp$breaks 

coul <- rainbow(length(seuil)-1, s=seq(from=0.05,1, length.out = length(seuil)-1),start=0.7, end=0.8)


windows(11, 10) 
par(mar=c(4.5,4.5,2,1)) 

plot(c(-5,10),c(41,51.5),xlab="longitude", ylab="latitude", col=NULL, main="Esp�rance de vie � la naissance")

for(i in 1:length (seuil) -1) {
map("france", MyData$Dep[MyData$Esp > seuil[i]], exact = F, fill = TRUE, col = coul[i], add=T) 
}

legend(-5,42, paste(">", seuil[1: length(seuil)-1], sep=""), 
	fill=coul, cex=0.75, title="nombre d'ann�es", h=T)



