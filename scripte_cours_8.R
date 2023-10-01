### Distributions de probabilt�
# Tests statistiques
#############################

# Test binomial
# 

#densit� de probabilit�: ex.
# 15 lanc�s de piece

#prob d'obtenir 7 fois pile (ou face)

#prob d'obtenir 12 fois pile
dbinom(12, size=15, prob=0.5)	# 1.4% de chance
# verif
plot(x, type="h") 
points(12, dbinom(12, size=15, prob=0.5), col=3,pch=19)

## tester le resultat: test binomial
# H0: l'essai est conforme � l'attente th�orique: p value doit �tre proche de 1
# H1: l'essai n'est pas conforme � l'attente th�orique: p value doit �tre proche de 0 (< 0.05)
binom.test(7, 15, p = 0.5)	# 46% de succes (en th�orie c'est 50%)

binom.test(12, 15, p = 0.5)	# 80% de succes (en th�orie c'est 50% de succes)



### Exercice
#On m�ne une enqu�te dans 10 quartiers diff�rents. Dans chaque quartier 135 personnes
#sont interrog�es sur leur accord pour l'installation d'un centre commercial � proximit�.
#Le questionnaire propose 3 r�ponses oui/non/ne sait pas, mais une seule doit �tre choisie

#Dans un quartier, nous avons obtenu:
#45 oui, 52 non et 38 ne sait pas

#Dans un autre, nous avons obtenu:
#83 oui, 32 non et 20 ne sait pas

#Selon le test binomial, y a t il un quartier dont les r�ponses sont proches ou au contraire �loign�es
#du mod�le th�orique de probabilit�?





#Representer graphiquement la fonction th�orique




#prob d'obtenir 7 fois pile (ou face)

# Test de poisson #
#En moyenne, 40 passagers prennent le bus � l�arr�t A le lundi matin.

#Quelle est la probabilit� qu�il y ait exactement 50 passagers 
#qui prennent le bus un lundi matin pris au hasard?

dpois(50, lambda=40)	# 

# Verif. de 0 � 100 passagers
x <- dpois(0:100, lambda=40)

plot(x, type="n",xlab="nombre de passagers", ylab="probabilit� (%)")
lines(x, type="h", col="darkgreen")

sum(x)
# probabilit� entre 30 et 50 passagers?
sum(x[30:50])	# 90% de chance

# test de poisson
# H0: l'�v�nement est conforme � l'attente th�orique: p value doit �tre proche de 1
# H1: l'�v�nement n'est pas conforme � l'attente th�orique: p value doit �tre proche de 0 (<0.05)

# pour 35 passagers
poisson.test(x=35, T=40)
35/40
40/40 # th�orique dans l'intervalle de confiance 

# pour 60 passagers
poisson.test(x=60, T=40)
60/40	
# th�orique en dehors l'intervalle de confiance 

# Exercice
# En moyenne 38 personnes par heure sont enregistr�es aux urgences d'un hopital
# Un jour, ont �t� enregistr�es 218 personnes sur un laps de temps de 5 heures.
# Est-ce conforme � l'attente th�orique?


#REPONSE
poisson.test(x=218, T=38*5)

####

binom(7, size=15, prob=1/2)	# 20% de chance
# verif
x <- round(dbinom(1:15, size=15, prob=1/2), 3)
sum(x)
x 
plot(x, type="h") 
points(7, dbinom(7, size=15, prob=0.5), col=2,pch=19)

# Test du Chi2 #
# tableau de contingence - exemple
# existe-t-il un lien significatif entre le parti auquel appartiennent les �lus et le genre (H/F)? 
# exemple
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
M

# test du Chi2
# H0: la proportion H/F est relativement �quilibr�e pour l'ensemble des partis: : p value doit �tre proche de 1 
# H1: la proportion H/F est in�gale selon les partis: p value doit �tre proche de 0 (<0.05)
chisq.test(M) 

plot(density(rchisq(1000, df=2)))	# 1000 valeurs, df=degr� de libert�

## tester des proportions
## H0: pour 4  populations de patients, on a une proportion de fumeurs �quivalente. p value proche de 1
## H1:  les proportions sont differentes dans au moins un cas. p.value proche de 0

PP <- data.frame(patients = c(86, 93, 136, 82), smokers = c(83, 90, 129, 70))
PP
dim(PP)
prop.test(PP$smokers,PP$patients)

## Exercice
# nous consid�rons l'offre m�dicale dans 5 villes 
# nombre de m�decins: 54, 28, 65, 42, 38
# dont sp�cialistes: 19, 10, 22, 14, 13
## H0: pour les 5 villes on a une proportion de sp�cialistes �quivalente. p value proche de 1
## H1:  les proportions sont differentes dans au moins un cas. p.value proche de 0

PP <- data.frame(patients = c(54, 28, 65, 42,38), smokers = c(19, 10, 22, 14,4))
PP
dim(PP)
prop.test(PP$smokers,PP$patients)

# Test de student

# Exemple tester si une difference dans les moyennes est significative
data()
# donn�es s�rie annuelle du niveau du lac Huron entre 1875�1972 (unit�: feet par rapport au niveau de la mer)

LK <- LakeHuron
length(LK)
plot(LK)

# on constate une tendance � la baisse- on va v�rifier si la moyenne du niveau du lac au cours de la premi�re periode
# est similaire � celle de la seconde periode ou au contraire, s'il existe une diff�rence significative.

LK1 <- LK[1:49]
LK2 <- LK[50:98]

LL1 <- length(LK1) ; LL1
LL2 <- length(LK2) ; LL2

MM1 <- mean(LK1) ; MM1
MM2 <- mean(LK2) ; MM2

MM2-MM1

## t. test
# H0: la diff�rence des moyennes est �gale � 0, c'est � dire pas de diff�rence >>> p value proche de 1
# H1: la diff�rence est significativement diff�rente de 0 >>> p. value proche de 0 (< 0.05)

t.test(LK2,LK1, alternative = "two.sided")		# on peut utiliser aussi 2 series de longueur diff�rente, �a marche aussi!!

# Verif
plot(density (rt (100000, df=90))) 	# 100000 valeurs, df=degr� de libert�

# test de corr�lation- t.test
# H0: la corr�lation entre les 2 s�ries est �gale � 0, c'est � dire pas de cor. >>> p value proche de 1
# H1: la corr�lation est significativement diff�rente de 0 >>> p. value proche de 0 (< 0.05)


Temp <- scan("Temperature_Huron_1875-1972.txt", sep=";")	# unit�: degr� C
Temp
length(Temp)

plot(Temp)

cor.test (LK, Temp) # correlation significatif avec un p value trés faible et un cofficient moyen et négatif

# Exercice
#Nile : Measurements of the annual flow of the river Nile at Aswan (formerly Assuan), 
#1871�1970, in 10^8 m^3, �with apparent changepoint near 1898� (Cobb(1978), Table 1, p.249). 
# unit�: milliard de m3 par an
data()

NN <- Nile
plot(NN)

##reponse
t.test(NN[1:28],NN[29:100])

## test de Fisher
# test diff�rence dans les variances
# H0: la diff�rence entre les variances des 2 s�ries est �gale � 0 (ou proch de 0 >>> p value proche de 1
# H1: la diff�rence entre les variances des 2 s�ries n'est pas �gale � 0 (ou proche de 0)>>> p. value proche de 0 (< 0.05)

SS1 <- var(LK1) ; SS1
SS2 <- var(LK2) ; SS2

SS2 - SS1

SS1/SS2

var.test (LK1, LK2) # on peut utiliser aussi 2 series de longueur diff�rente, �a marche aussi!!


# Verif
plot(density (rf (1000,48,48)))	# 1000 valeurs, 48=degr� de libert� (df)
abline(v=0.61, col=2)
abline(v= qf (0.05,48,48), col=3)# intervalle 90%
abline(v= qf (0.95,48,48), col=3)

abline(v= qf (0.025,48,48), col=4)# intervalle 95%
abline(v= qf (0.975,48,48), col=4)

abline(v= qf (0.01,48,48), col=6)# intervalle 99%
abline(v= qf (0.99,48,48), col=6)

# Test de Shapiro
# H0 distribution suit une loi normale >>> p val = 1
# H1 distribution ne suit pas une loi normale >>> p val = 0
# ex: 
RR1 <- rnorm(1000, mean=10, sd=2)
hist(RR1)
shapiro.test(RR1)

RR2 <- rgamma(1000,shape=1,rate=0.8)
hist(RR2)
shapiro.test(RR2)

# Test de Kolmogorov-Smirnov
# H0 les 2 distributions suivent une m�me loi >>> p val = 1 
# H1 les 2 distributions ne suivent pas une m�me loi >>> p val = 0

ks.test(RR1,RR2)

