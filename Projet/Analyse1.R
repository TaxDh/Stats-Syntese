library(psych)
library(ggstatsplot)

mathematique <- read.table("student-mat.csv", header = T, sep = ",")
View(mathematique)

portugais <- read.table("student-por.csv", header = T, sep = ",")
View(portugais)
dim(portugais)


######################################################################################################
#je trouve qu'il y a vraiment trop de 0 dans les données, c'est ma démarche ici
# Créer un histogramme pour la variable G3
hist(mathematique$G1, 
     main = "Histogramme de la variable G3",
     xlab = "Valeurs de G3",
     ylab = "Fréquence",
     col = "skyblue",
     border = "white")
# Créer un histogramme pour la variable G3
hist(mathematique$G2, 
     main = "Histogramme de la variable G3",
     xlab = "Valeurs de G3",
     ylab = "Fréquence",
     col = "skyblue",
     border = "white")
# Créer un histogramme pour la variable G3
hist(mathematique$G3, 
     main = "Histogramme de la variable G3",
     xlab = "Valeurs de G3",
     ylab = "Fréquence",
     col = "skyblue",
     border = "white")


# Créer un histogramme pour la variable G3
hist(portugais$G1, 
     main = "Histogramme de la variable G3",
     xlab = "Valeurs de G3",
     ylab = "Fréquence",
     col = "skyblue",
     border = "white")

hist(portugais$G2, 
     main = "Histogramme de la variable G3",
     xlab = "Valeurs de G3",
     ylab = "Fréquence",
     col = "skyblue",
     border = "white")

hist(portugais$G3, 
     main = "Histogramme de la variable G3",
     xlab = "Valeurs de G3",
     ylab = "Fréquence",
     col = "skyblue",
     border = "white")


print(mathematique$G3[mathematique$G3 >= 0 & mathematique$G3 <= 5])
print(mathematique$G2[mathematique$G2 >= 0 & mathematique$G2 <= 5])
print(mathematique$G1[mathematique$G1 >= 0 & mathematique$G1 <= 5])
print(portugais$G1[portugais$G1 >= 0 & portugais$G1 <= 5])
print(portugais$G2[portugais$G2 >= 0 & portugais$G2 <= 5])
print(portugais$G3[portugais$G3 >= 0 & portugais$G3 <= 5])

#Fin de la démarche pour les 0, on va éliminer les 0 des données
###################################################################


##################################################################
#Élimination des 0 dans les données

which(mathematique$G3 == 0)
mathematique <- mathematique[mathematique$G3 != 0, ]
which(mathematique$G3 == 0)
which(mathematique$G2 == 0)
which(mathematique$G1 == 0)

which(portugais$G3 == 0)
portugais <- portugais[portugais$G3 != 0, ]
which(portugais$G3 == 0)
which(portugais$G2 == 0)
which(portugais$G1 == 0)

#Fin de l'élimination des 0 dans les données
##############################################

##############################################
mTravelTime <- mathematique$traveltime
pTravelTime <- portugais$traveltime

mStudyTime <- mathematique$studytime
pStudyTime <- portugais$studytime

mFailures <- mathematique$failures
pFailures <- portugais$failures

mSchoolSup <- mathematique$schoolsup
pSchoolSup <- portugais$schoolsup

mFamSup <- mathematique$famsup
pFamSup <- portugais$famsup

mPaid <- mathematique$paid
pPaid <- portugais$paid

mInternet <- mathematique$internet
pInternet <- portugais$internet

mFreeTime <- mathematique$freetime
pFreeTime <- portugais$freetime

mDalc <- mathematique$Dalc
PDalc <- portugais$Dalc

mWalc <- mathematique$Walc
pWalc <- portugais$Walc

mAbsences <- mathematique$absences
pAbsences <- portugais$absences

mG3 <- mathematique$G3
pG3 <- portugais$G3


##############################################






###################################################
#Début de l'analyse

# analyse du temps pour aller à l'école en premier
psych::describeBy(mG3, group = mTravelTime)

#la variance semble etre ok sans plus, peut être un peu différente


ggstatsplot::ggbetweenstats(
  data = mathematique, x = traveltime, y = G3,
  title = "La note finale en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
)


#Les variances sont clairement différentes et les n_i sont problématique, on devrait faire un test de randomisation si possible


mathematique$logG3 <- log(mG3+2)

ggstatsplot::ggbetweenstats(
  data = mathematique, x = traveltime, y = logG3,
  title = "La note finale en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = T,
  bf.message = FALSE,
)

#Le log n'aide pas, mais vérifions tout de même la normalité

qqnorm(mG3[mTravelTime == 1])
qqline(mG3[mTravelTime == 1], col = "red")

#cela semble normal, faisons un test de welch pour la variable traveltime en fonction de g3

resultat_welch <- oneway.test(G3 ~ traveltime, data = mathematique, var.equal = FALSE)
resultat_welch

###### pour le cours de mathématique il n'y a aucune corrélation entre le temps de voyage et la note finale G3.

#Maintenant on vérifie pour portugais

# analyse du temps pour aller à l'école en premier
psych::describeBy(pG3, group = pTravelTime)

#la variance semble très semblable, peut être un peu différente


ggstatsplot::ggbetweenstats(
  data = portugais, x = traveltime, y = G3,
  title = "La note finale en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
)

#La variance est assez différente, surtout à cause d'une valeur aberrante, mais même en l'enlevant cela serait un problème



portugais$racineG3 <- sqrt(portugais$G3)


ggstatsplot::ggbetweenstats(
  data = portugais, x = traveltime, y = racineG3,
  title = "La note finale en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
)
#une transformation racine carré ne fonctionne pas à cause du niveau 1 et de 3 valeurs aberrantes dans le niveau 1.

#Vérifions la normalité

qqnorm(mG3[mTravelTime == 1])
qqline(mG3[mTravelTime == 1], col = "red")

#il y a une bonne normalité, on va donc refaire un test de welch

resultat_welch <- oneway.test(G3 ~ traveltime, data = portugais, var.equal = FALSE)
resultat_welch

########### Pour le cours de français, il semble y aoir une corrélation entre le temps de voyage et la note finale G3

####################################################################################################################
#Nous somme rendu à la variable StudyTime

psych::describeBy(mG3, group = mStudyTime)

#Les variances ont l'air pas mal égale, vérifions visuellement


ggstatsplot::ggbetweenstats(
  data = mathematique, x = studytime, y = G3,
  title = "La note finale en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = T,
  bf.message = FALSE,
)

#Les variances sont pas mal égale, 

qqnorm(mG3[mStudyTime == 1])
qqline(mG3[mStudyTime == 1], col = "red")




# Méthode de Bonferroni

# Création de sous-ensembles pour chaque niveau de studytime
groupe1 <- mG3[mStudyTime == 1]
groupe2 <- mG3[mStudyTime == 2]
groupe3 <- mG3[mStudyTime == 3]
groupe4 <- mG3[mStudyTime == 4]

# Effectuer des tests t pour chaque paire de niveaux de studytime
p_values <- c(
  t.test(groupe1, groupe2)$p.value,
  t.test(groupe1, groupe3)$p.value,
  t.test(groupe1, groupe4)$p.value,
  t.test(groupe2, groupe3)$p.value,
  t.test(groupe2, groupe4)$p.value,
  t.test(groupe3, groupe4)$p.value
)

# Ajuster les p-valeurs pour les comparaisons multiples (Bonferroni)
p_values_adjusted <- p.adjust(p_values, method = "bonferroni")

# Afficher les résultats ajustés
p_values_adjusted

# p_values_adjusted: [1] 1.000000000 0.241111816 0.902398797 0.007709908 0.279629521 1.000000000

#il semble que le niveau 3 d'étude est différent du niveau 2
#mais le niveau 1 ne l'est pas du niveau 3.
#en fait le niveau 2 à une moyenne inférieur au niveau 1...




####-------------------------------####
#Maintenant portugais


psych::describeBy(pG3, group = pStudyTime)

#Les variances ont l'air pas mal égale, vérifions visuellement


ggstatsplot::ggbetweenstats(
  data = mathematique, x = studytime, y = G3,
  title = "La note finale en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = T,
  bf.message = FALSE,
)

#Les variances sont pas mal égale, 

qqnorm(pG3[pStudyTime == 1])
qqline(pG3[pStudyTime == 1], col = "red")



groupe1 <- pG3[pStudyTime == 1]
groupe2 <- pG3[pStudyTime == 2]
groupe3 <- pG3[pStudyTime == 3]
groupe4 <- pG3[pStudyTime == 4]

# Effectuer des tests t pour chaque paire de niveaux de studytime
p_values <- c(
  t.test(groupe1, groupe2)$p.value,
  t.test(groupe1, groupe3)$p.value,
  t.test(groupe1, groupe4)$p.value,
  t.test(groupe2, groupe3)$p.value,
  t.test(groupe2, groupe4)$p.value,
  t.test(groupe3, groupe4)$p.value
)

# Ajuster les p-valeurs pour les comparaisons multiples (Bonferroni)
p_values_adjusted <- p.adjust(p_values, method = "bonferroni")

# Afficher les résultats ajustés
p_values_adjusted

# p_values_adjusted: [1] 1.387449e-05 7.755940e-09 1.180120e-02 2.895336e-02 1.000000e+00 1.000000e+00


#######################################
# Maintenant la variable SchoolSup

psych::describeBy()










































