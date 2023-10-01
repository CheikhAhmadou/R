# Distributions de probabilté
# Tests statistiques
#############################

# Test binomial
# 

#densité de probabilité: ex.
# 15 lancés de piece

#prob d'obtenir 7 fois pile (ou face)
dbinom(7, size=15, prob=1/2)	# 20% de chance
# verif
x <- round(dbinom(1:15, size=15, prob=1/2), 3)
sum(x)
x
plot(x, type="h") 
points(7, dbinom(7, size=15, prob=0.5), col=2,pch=19)


#prob d'obtenir 12 fois pile
dbinom(12, size=15, prob=0.5)	# 1.4% de chance
# verif
plot(x, type="h") 
points(12, dbinom(12, size=15, prob=0.5), col=3,pch=19)

## tester le resultat: test binomial
# H0: l'essai est conforme à l'attente théorique: p value doit être proche de 1
# H1: l'essai n'est pas conforme à l'attente théorique: p value doit être proche de 0 (< 0.05)
binom.test(7, 15, p = 0.5)	# 46% de succes (en théorie c'est 50%)

binom.test(12, 15, p = 0.5)	# 80% de succes (en théorie c'est 50% de succes)



### Exercice
#On mène une enquête dans 10 quartiers différents. Dans chaque quartier 135 personnes
#sont interrogées sur leur accord pour l'installation d'un centre commercial à proximité.
#Le questionnaire propose 3 réponses oui/non/ne sait pas, mais une seule doit être choisie

#Dans un quartier, nous avons obtenu:
#45 oui, 52 non et 38 ne sait pas

#Dans un autre, nous avons obtenu:
#83 oui, 32 non et 20 ne sait pas

#Selon le test binomial, y a t il un quartier dont les réponses sont proches ou au contraire éloignées
#du modèle théorique de probabilité?

 



#Representer graphiquement la fonction théorique



############################
# Test de poisson #
#En moyenne, 40 passagers prennent le bus à l’arrêt A le lundi matin.

#Quelle est la probabilité qu’il y ait exactement 50 passagers 
#qui prennent le bus un lundi matin pris au hasard?

dpois(50, lambda=40)	# 

# Verif. de 0 à 100 passagers
x <- dpois(0:100, lambda=40)

plot(x, type="n",xlab="nombre de passagers", ylab="probabilité (%)")
lines(x, type="h", col="darkgreen")

sum(x)
# probabilité entre 30 et 50 passagers?
sum(x[30:50])	# 90% de chance

# test de poisson
# H0: l'évènement est conforme à l'attente théorique: p value doit être proche de 1
# H1: l'évènement n'est pas conforme à l'attente théorique: p value doit être proche de 0 (<0.05)

# pour 35 passagers
poisson.test(x=35, T=40)
35/40
40/40 # théorique dans l'intervalle de confiance 

# pour 60 passagers
poisson.test(x=60, T=40)
60/40	
# théorique en dehors l'intervalle de confiance 

#####
# Exercice
# En moyenne 38 personnes par heure sont enregistrées aux urgences d'un hopital
# Un jour, ont été enregistrées 218 personnes sur un laps de temps de 5 heures.
# Est-ce conforme à l'attente théorique?





############################
# Test du Chi2 #
# tableau de contingence - exemple
# existe-t-il un lien significatif entre le parti auquel appartiennent les élus et le genre (H/F)? 
# exemple
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
M

# test du Chi2
# H0: la proportion H/F est relativement équilibrée pour l'ensemble des partis: : p value doit être proche de 1 
# H1: la proportion H/F est inégale selon les partis: p value doit être proche de 0 (<0.05)
chisq.test(M) 

plot(density(rchisq(1000, df=2)))	# 1000 valeurs, df=degré de liberté


#
## tester des proportions
## H0: pour 4  populations de patients, on a une proportion de fumeurs équivalente. p value proche de 1
## H1:  les proportions sont differentes dans au moins un cas. p.value proche de 0

PP <- data.frame(patients = c(86, 93, 136, 82), smokers = c(83, 90, 129, 70))
PP
dim(PP)
prop.test(PP$smokers,PP$patients)

## Exercice
# nous considérons l'offre médicale dans 5 villes 
# nombre de médecins: 54, 28, 65, 42, 38
# dont spécialistes: 19, 10, 22, 14, 13
## H0: pour les 5 villes on a une proportion de spécialistes équivalente. p value proche de 1
## H1:  les proportions sont differentes dans au moins un cas. p.value proche de 0






##################
# Test de student

# Exemple tester si une difference dans les moyennes est significative
data()
# données série annuelle du niveau du lac Huron entre 1875–1972 (unité: feet par rapport au niveau de la mer)

LK <- LakeHuron
length(LK)
plot(LK)

# on constate une tendance à la baisse- on va vérifier si la moyenne du niveau du lac au cours de la première periode
# est similaire à celle de la seconde periode ou au contraire, s'il existe une différence significative.

LK1 <- LK[1:49]
LK2 <- LK[50:98]

LL1 <- length(LK1) ; LL1
LL2 <- length(LK2) ; LL2

MM1 <- mean(LK1) ; MM1
MM2 <- mean(LK2) ; MM2

MM2-MM1

## t. test
# H0: la différence des moyennes est égale à 0, c'est à dire pas de différence >>> p value proche de 1
# H1: la différence est significativement différente de 0 >>> p. value proche de 0 (< 0.05)

t.test(LK2,LK1, alternative = "two.sided")		# on peut utiliser aussi 2 series de longueur différente, ça marche aussi!!

# Verif
plot(density (rt (100000, df=90))) 	# 100000 valeurs, df=degré de liberté

# test de corrélation- t.test
# H0: la corrélation entre les 2 séries est égale à 0, c'est à dire pas de cor. >>> p value proche de 1
# H1: la corrélation est significativement différente de 0 >>> p. value proche de 0 (< 0.05)


Temp <- scan("Temperature_Huron_1875-1972.txt", sep=";")	# unité: degré C
Temp
length(Temp)

plot(Temp)

cor.test (LK, Temp)


# Exercice
#Nile : Measurements of the annual flow of the river Nile at Aswan (formerly Assuan), 
#1871–1970, in 10^8 m^3, “with apparent changepoint near 1898” (Cobb(1978), Table 1, p.249). 
# unité: milliard de m3 par an
data()
 
NN <- Nile
plot(NN)





## test de Fisher
# test différence dans les variances
# H0: la différence entre les variances des 2 séries est égale à 0 (ou proch de 0 >>> p value proche de 1
# H1: la différence entre les variances des 2 séries n'est pas égale à 0 (ou proche de 0)>>> p. value proche de 0 (< 0.05)

SS1 <- var(LK1) ; SS1
SS2 <- var(LK2) ; SS2

SS2 - SS1

SS1/SS2

var.test (LK1, LK2) # on peut utiliser aussi 2 series de longueur différente, ça marche aussi!!

# Verif
plot(density (rf (1000,48,48)))	# 1000 valeurs, 48=degré de liberté (df)
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
# H0 les 2 distributions suivent une même loi >>> p val = 1 
# H1 les 2 distributions ne suivent pas une même loi >>> p val = 0

ks.test(RR1,RR2)


