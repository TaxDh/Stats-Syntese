library(psych)
library(ggstatsplot)

#données: https://www.kaggle.com/datasets/uciml/student-alcohol-consumption
mathematique <- read.table("student-mat.csv", header = T, sep = ",")
#View(mathematique)

portugais <- read.table("student-por.csv", header = T, sep = ",")
#View(portugais)
dim(portugais)


######################################################################################################
#je trouve qu'il y a vraiment trop de 0 dans les données, c'est ma démarche ici
# Créer un histogramme pour la variable G1
hist(mathematique$G1, 
     main = "Histogramme de la variable G1",
     xlab = "Valeurs de G1",
     ylab = "Fréquence",
     col = "skyblue",
     border = "white")
# Créer un histogramme pour la variable G1
hist(mathematique$G2, 
     main = "Histogramme de la variable G2",
     xlab = "Valeurs de G2",
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


# Créer un histogramme pour la variable G1
hist(portugais$G1, 
     main = "Histogramme de la variable G1",
     xlab = "Valeurs de G1",
     ylab = "Fréquence",
     col = "skyblue",
     border = "white")

hist(portugais$G2, 
     main = "Histogramme de la variable G2",
     xlab = "Valeurs de G2",
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

which(mathematique$G3 < 6)
mathematique <- mathematique[mathematique$G3 >= 5, ]
which(mathematique$G3 < 5)
which(mathematique$G2 == 0)
which(mathematique$G1 == 0)

which(portugais$G3 < 6)
portugais <- portugais[portugais$G3 >= 5, ]
which(portugais$G3 <6 )
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
  title = "La note finale en mathématique en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)


#Les variances sont clairement différentes et les n_i sont problématique, on devrait faire un test de randomisation si possible


mathematique$logG3 <- log(mG3)

ggstatsplot::ggbetweenstats(
  data = mathematique, x = traveltime, y = logG3,
  title = "Le logarightme de la note finale en mathématique en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = T,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#Le log n'aide pas, mais vérifions tout de même la normalité

qqnorm(mG3[mTravelTime == 1])
qqline(mG3[mTravelTime == 1], col = "red")
qqnorm(mG3[mTravelTime == 2])
qqline(mG3[mTravelTime == 2], col = "red")
qqnorm(mG3[mTravelTime == 3])
qqline(mG3[mTravelTime == 3], col = "red")
qqnorm(mG3[mTravelTime == 4])
qqline(mG3[mTravelTime == 4], col = "red")

#Les données semblent suivre une loi normales

# Méthode de Bonferroni

# Création de sous-ensembles pour chaque niveau de studytime
groupe1 <- mG3[mTravelTime == 1]
groupe2 <- mG3[mTravelTime == 2]
groupe3 <- mG3[mTravelTime == 3]
groupe4 <- mG3[mTravelTime == 4]

p_values <- c(
  t.test(groupe1, groupe2, var.equal = F)$p.value,
  t.test(groupe1, groupe3, var.equal = F)$p.value,
  t.test(groupe1, groupe4, var.equal = F)$p.value,
  t.test(groupe2, groupe3, var.equal = F)$p.value,
  t.test(groupe2, groupe4, var.equal = F)$p.value,
  t.test(groupe3, groupe4, var.equal = F)$p.value
)

# Ajuster les p-valeurs pour les comparaisons multiples (Bonferroni)
p_values_adjusted <- p.adjust(p_values, method = "bonferroni")

# Afficher les résultats ajustés
p_values_adjusted

###### pour le cours de mathématique il n'y a aucune corrélation entre le temps de voyage et la note finale G3.

#Maintenant on vérifie pour portugais

# analyse du temps pour aller à l'école en premier
psych::describeBy(pG3, group = pTravelTime)

#la variance semble très semblable, peut être un peu différente


ggstatsplot::ggbetweenstats(
  data = portugais, x = traveltime, y = G3,
  title = "La note finale en portugais en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#La variance est assez différente, essayons une transformation racine carré



portugais$racineG3 <- sqrt(portugais$G3)


ggstatsplot::ggbetweenstats(
  data = portugais, x = traveltime, y = racineG3,
  title = "La racine carré de la note finale en portugais en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)
#ça améliore un peu, essayons avec une transformation log, voir ce qui est le mieux

portugais$logG3 <- log(portugais$G3)

ggstatsplot::ggbetweenstats(
  data = portugais, x = traveltime, y = racineG3,
  title = "Le logarithme de la note finale en portugais en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#même effet que la racine carré.

#Vérifions la normalité

qqnorm(pG3[pTravelTime == 1])
qqline(pG3[pTravelTime == 1], col = "red")
qqnorm(pG3[pTravelTime == 2])
qqline(pG3[pTravelTime == 2], col = "red")
qqnorm(pG3[pTravelTime == 3])
qqline(pG3[pTravelTime == 3], col = "red")
qqnorm(pG3[pTravelTime == 4])
qqline(pG3[pTravelTime == 4], col = "red")

#Les données semblent suivre une loi normale

#réutilisons la méthode de bonferonni avec un test de welch


groupe1 <- pG3[pTravelTime == 1]
groupe2 <- pG3[pTravelTime == 2]
groupe3 <- pG3[pTravelTime == 3]
groupe4 <- pG3[pTravelTime == 4]

p_values <- c(
  t.test(groupe1, groupe2, var.equal = F)$p.value,
  t.test(groupe1, groupe3, var.equal = F)$p.value,
  t.test(groupe1, groupe4, var.equal = F)$p.value,
  t.test(groupe2, groupe3, var.equal = F)$p.value,
  t.test(groupe2, groupe4, var.equal = F)$p.value,
  t.test(groupe3, groupe4, var.equal = F)$p.value
)

# Ajuster les p-valeurs pour les comparaisons multiples (Bonferroni)
p_values_adjusted <- p.adjust(p_values, method = "bonferroni")

# Afficher les résultats ajustés
p_values_adjusted

########### Pour le cours de français, il semble y aoir une corrélation entre le temps de voyage et la note finale G3
#Mais cette corrélation est très faible, à peine inférieur à 0.05
#[1] 0.40263836 0.14093051 0.04318712 1.00000000 0.25417042 1.00000000, c'est entre le groupe 1 et 4.
#Selon le graphique, ça indiquerait que le pire temps de voyages est significatif avec le plus petit temps.

####################################################################################################################
#Nous somme rendu à la variable StudyTime

psych::describeBy(mG3, group = mStudyTime)

#Les variances ont l'air pas mal égale, vérifions visuellement


ggstatsplot::ggbetweenstats(
  data = mathematique, x = studytime, y = G3,
  title = "La note finale en mathématique en fonction du temps d'étude",
  mean.ci = TRUE,
  type="p",
  var.equal = T,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#Les variances sont pas mal égale, 

qqnorm(mG3[mStudyTime == 1])
qqline(mG3[mStudyTime == 1], col = "red")
qqnorm(mG3[mStudyTime == 2])
qqline(mG3[mStudyTime == 2], col = "red")
qqnorm(mG3[mStudyTime == 3])
qqline(mG3[mStudyTime == 3], col = "red")
qqnorm(mG3[mStudyTime == 4])
qqline(mG3[mStudyTime == 4], col = "red")

#les données suivent une loi normales


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

# p_values_adjusted: [1] 1.000000000 0.241111816 0.902398797 0.009943247 0.310055949 1.000000000

#il semble que le niveau 3 d'étude est différent du niveau 2
#mais le niveau 1 ne l'est pas du niveau 3.
#en fait le niveau 2 à une moyenne inférieur au niveau 1...




####-------------------------------####
#Maintenant portugais


psych::describeBy(pG3, group = pStudyTime)

#Les variances ont l'air pas mal égale, vérifions visuellement


ggstatsplot::ggbetweenstats(
  data = portugais, x = studytime, y = G3,
  title = "La note finale en portugais en fonction du temps d'étude",
  mean.ci = TRUE,
  type="p",
  var.equal = T,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#Les variances sont pas mal égale, 

qqnorm(pG3[pStudyTime == 1])
qqline(pG3[pStudyTime == 1], col = "red")
qqnorm(pG3[pStudyTime == 2])
qqline(pG3[pStudyTime == 2], col = "red")
qqnorm(pG3[pStudyTime == 3])
qqline(pG3[pStudyTime == 3], col = "red")
qqnorm(pG3[pStudyTime == 4])
qqline(pG3[pStudyTime == 4], col = "red")

#Les données semblent suivent la loi normale

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

# p_values_adjusted: [1] 4.641942e-06 7.755940e-09 1.180120e-02 3.998751e-02 1.000000e+00 1.000000e+00


#######################################
# Maintenant la variable SchoolSup

psych::describeBy(mG3, group = mSchoolSup)

#grosse différence dans les variances

ggstatsplot::ggbetweenstats(
  data = mathematique, x = schoolsup, y = G3,
  title = "La note finale en mathématique fonction de l'aide supplémentaire à l'école",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#encore une grosse différence dans les variances, essayons une racine carré
mathematique$racineG3 <- sqrt(mathematique$G3)


ggstatsplot::ggbetweenstats(
  data = mathematique, x = schoolsup, y = racineG3,
  title = "La racine carré de la note finale en mathématique en fonction de l'aide supplémentaire à l'école",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)#je n'ai pas ce graphique

#ça améliore les choses
#Testons la normalité

qqnorm(mG3[mSchoolSup == "yes"])
qqline(mG3[mSchoolSup == "yes"], col = "red")
qqnorm(mG3[mSchoolSup == "no"])
qqline(mG3[mSchoolSup == "no"], col = "red")

#Les données suivent une loi normale


#ça améliore un peu les données, faisons un test t

t.test(racineG3 ~ schoolsup, data = mathematique, var.equal = T)


#p-value = 2.831e-06, donc oui l'école en plus est très significative, mais ça n'améliore pas la note, au contraire.

#------------------------
#Maintenant portugais

psych::describeBy(pG3, group = pSchoolSup)

#grosse différence dans les variances

ggstatsplot::ggbetweenstats(
  data = portugais, x = schoolsup, y = G3,
  title = "La note finale en portugais en fonction de l'aide supplémentaire à l'école",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#assez grosse différence dans les variances, essayons un log

portugais$logG3 <- log(portugais$G3)


ggstatsplot::ggbetweenstats(
  data = portugais, x = schoolsup, y = logG3,
  title = "Le logarithme de la note finale en portugais en fonction de l'aide supplémentaire à l'école",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#N'améliore pas les variances


qqnorm(pG3[pSchoolSup == "yes"])
qqline(pG3[pSchoolSup == "yes"], col = "red")
qqnorm(pG3[pSchoolSup == "no"])
qqline(pG3[pSchoolSup == "no"], col = "red")

#ça suit une loi normale, donc faisons un test de welch

t.test(G3 ~ schoolsup, data = portugais, var.equal = F)

#C'est significatif (p-value = 0.001166), mais l'aide à l'école n'aide pas vraiment les notes.


#############################################################
#On est rendu à la variable mFamSup
############################################################

psych::describeBy(mG3, group = mFamSup)

#variance très proche, regardons visuellement


ggstatsplot::ggbetweenstats(
  data = mathematique, x = famsup, y = G3,
  title = "La note finale en mathématique en fonction de l'aide supplémentaire apporté par la famille",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#variance très semblable aussi, vérifions la normalité



qqnorm(mG3[mFamSup == "yes"])
qqline(mG3[mFamSup == "yes"], col = "red")
qqnorm(mG3[mFamSup == "no"])
qqline(mG3[mFamSup == "no"], col = "red")


#Les données suivent une loi normale, donc faisons un test t

t.test(G3 ~ famsup, data = mathematique, equal.var = T)

#p-value = 0.1469, donc il n'y a pas de différence avec la famille qui aide à la maison pour les math

#------------------------------------------#
# Portugais

psych::describeBy(pG3, group = pFamSup)

#Variance quasi identique

ggstatsplot::ggbetweenstats(
  data = portugais, x = famsup, y = G3,
  title = "La note finale en portugais en fonction de l'aide supplémentaire apporté par la famille",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#Variance quasi identique

qqnorm(pG3[pFamSup == "yes"])
qqline(pG3[pFamSup == "yes"], col = "red")
qqnorm(pG3[pFamSup == "no"])
qqline(pG3[pFamSup == "no"], col = "red")

#suivent loi normale

t.test(G3 ~ famsup, data = portugais, equal.var = T)

#p-value = 0.9683, pour le portugais également, ça n'aide pas l'aide à la maison, ça ne change rien.

################################################################################
#Variable mPaid

psych::describeBy(mG3, group = mPaid)

#Variance très proche

ggstatsplot::ggbetweenstats(
  data = mathematique, x = paid, y = G3,
  title = "La note finale en mathématique fonction des cours supplémentaires privés",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#Variance toujours très proche

qqnorm(mG3[mPaid == "yes"])
qqline(mG3[mPaid == "yes"], col = "red")
qqnorm(mG3[mPaid == "no"])
qqline(mG3[mPaid == "no"], col = "red")


# Les données suivent une loi normale

t.test(G3 ~ paid, data = mathematique, var.equal = T)

#p-value = 0.6744, aucune différence pour mpaid

psych::describeBy(pG3, group = pPaid)

#Variance un peu éloigné

ggstatsplot::ggbetweenstats(
  data = portugais, x = paid, y = G3,
  title = "La note finale en portugais fonction des cours supplémentaires privés",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#Les variances sont assé éloigné
#Essayons une racine carré
ggstatsplot::ggbetweenstats(
  data = portugais, x = paid, y = racineG3,
  title = "La racine carré de la note finale en portugais fonction des cours supplémentaires privés",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#essayons un log
ggstatsplot::ggbetweenstats(
  data = portugais, x = paid, y = logG3,
  title = "Le logarithme de la note finale en portugais fonction des cours supplémentaires privés",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#aucune transformation ne fait vraiment mieux que les données sans transformations, vérifions la normalité



qqnorm(pG3[pPaid == "yes"])
qqline(pG3[pPaid == "yes"], col = "red")
qqnorm(pG3[pPaid == "no"])
qqline(pG3[pPaid == "no"], col = "red")

#Les données suivent une loi normale

t.test(G3 ~ paid, data = portugais, var.equal = F)
# p-value = 0.05082, on pourrait rejeter le test, mais on est presque pile sur le alpha = 5%, j'irais pour un rejet
#car les médiane sont égales et que la médiane résiste aux valeurs extrêmes, il y a aussi le fait que cela n'améliore pas
#la moyenne de la note, au pire, si c'était significatif, paid diminue la note moyenne des étudiants.


#############################################################
# Variable Internet

psych::describeBy(mG3, group = mInternet)

#Variance proche

ggstatsplot::ggbetweenstats(
  data = mathematique, x = internet, y = G3,
  title = "La note finale mathématique en fonction de l'accès à l'internet à domicile",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#Variance proche

ggstatsplot::ggbetweenstats(
  data = mathematique, x = internet, y = logG3,
  title = "Le logarithme de la note finale mathématique en fonction de l'accès à l'internet à domicile",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#variance encore plus proche

qqnorm(mG3[mInternet == "yes"])
qqline(mG3[mInternet == "yes"], col = "red")
qqnorm(mG3[mInternet == "no"])
qqline(mG3[mInternet == "no"], col = "red")


# Les données suivent une loi normale

t.test(logG3 ~ internet, data = mathematique, var.equal = T)

#p-value = 0.0353, l'internet semble montré une différence, elle semble aider!

psych::describeBy(pG3, group = pInternet)

#Variance peu éloigné

ggstatsplot::ggbetweenstats(
  data = portugais, x = internet, y = G3,
  title = "La note finale en portugais en fonction de l'accès à l'internet à domicile",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#Les variances sont proche

#vérifions la normalité



qqnorm(pG3[pInternet == "yes"])
qqline(pG3[pInternet == "yes"], col = "red")
qqnorm(pG3[pInternet == "no"])
qqline(pG3[pInternet == "no"], col = "red")

#Les données suivent une loi normale

t.test(G3 ~ internet, data = portugais, var.equal = T)

#p-value = 0.0009197, très significatif, l'internet semblent améliorer les notes de portugais


###############################################
# Variable mDalc

psych::describeBy(mG3, group = mDalc)

#Les variances semblent différent

ggstatsplot::ggbetweenstats(
  data = mathematique, x = Dalc, y = G3,
  title = "La note finale en mathématique en fonction de la consommation d'alcool les jours d'écoles",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

ggstatsplot::ggbetweenstats(
  data = mathematique, x = Dalc, y = logG3,
  title = "Le logarithme note finale en mathématique en fonction de la consommation d'alcool les jours d'écoles",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)


#la transformation log améliore, mais pas beaucoup
#on vérifie la normalité

qqnorm(mG3[mDalc == 1])
qqline(mG3[mDalc == 1], col = "red")
qqnorm(mG3[mDalc == 2])
qqline(mG3[mDalc == 2], col = "red")
qqnorm(mG3[mDalc == 3])
qqline(mG3[mDalc == 3], col = "red")
qqnorm(mG3[mDalc == 4])
qqline(mG3[mDalc == 4], col = "red")
qqnorm(mG3[mDalc == 5])
qqline(mG3[mDalc == 5], col = "red")

#les données suivent une loi normale, difficile d'exclure pour les niveaux 4 et 5 (peu d'observations)

groupe1 <- mG3[mDalc == 1]
groupe2 <- mG3[mDalc == 2]
groupe3 <- mG3[mDalc == 3]
groupe4 <- mG3[mDalc == 4]
groupe5 <- mG3[mDalc == 5]


p_values <- c(
  t.test(groupe1, groupe2, var.equal = F)$p.value,
  t.test(groupe1, groupe3, var.equal = F)$p.value,
  t.test(groupe1, groupe4, var.equal = F)$p.value,
  t.test(groupe1, groupe5, var.equal = F)$p.value,
  t.test(groupe2, groupe3, var.equal = F)$p.value,
  t.test(groupe2, groupe4, var.equal = F)$p.value,
  t.test(groupe2, groupe5, var.equal = F)$p.value,
  t.test(groupe3, groupe4, var.equal = F)$p.value,
  t.test(groupe3, groupe5, var.equal = F)$p.value
)

# Ajuster les p-valeurs pour les comparaisons multiples (Bonferroni)
p_values_adjusted <- p.adjust(p_values, method = "bonferroni")

# Afficher les résultats ajustés
p_values_adjusted


#0.3705751 1.0000000 0.5144004 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
#Il y a aucune différence significative, boire la semaine ne semblent pas changer les notes de mathématique

#-----------------------
#portugais

psych::describeBy(pG3, group = PDalc)

#Les variances sont assez différentes

ggstatsplot::ggbetweenstats(
  data = portugais, x = Dalc, y = G3,
  title = "La note finale en portugais en fonction de la consommation d'alcool les jours d'écoles",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)


ggstatsplot::ggbetweenstats(
  data = portugais, x = Dalc, y = logG3,
  title = "Le logarithme de la note finale en portugais en fonction de la consommation d'alcool les jours d'écoles",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#la transformation log améliore, mais pas beaucoup
#on vérifie la normalité

qqnorm(pG3[PDalc == 1])
qqline(pG3[PDalc == 1], col = "red")
qqnorm(pG3[PDalc == 2])
qqline(pG3[PDalc == 2], col = "red")
qqnorm(pG3[PDalc == 3])
qqline(pG3[PDalc == 3], col = "red")
qqnorm(pG3[PDalc == 4])
qqline(pG3[PDalc == 4], col = "red")
qqnorm(pG3[PDalc == 5])
qqline(pG3[PDalc == 5], col = "red")

#les données suivent une loi normale, difficile d'exclure pour les niveaux 4 et 5 (peu d'observations)

groupe1 <- pG3[PDalc == 1]
groupe2 <- pG3[PDalc == 2]
groupe3 <- pG3[PDalc == 3]
groupe4 <- pG3[PDalc == 4]
groupe5 <- pG3[PDalc == 5]


p_values <- c(
  t.test(groupe1, groupe2, var.equal = F)$p.value,
  t.test(groupe1, groupe3, var.equal = F)$p.value,
  t.test(groupe1, groupe4, var.equal = F)$p.value,
  t.test(groupe1, groupe5, var.equal = F)$p.value,
  t.test(groupe2, groupe3, var.equal = F)$p.value,
  t.test(groupe2, groupe4, var.equal = F)$p.value,
  t.test(groupe2, groupe5, var.equal = F)$p.value,
  t.test(groupe3, groupe4, var.equal = F)$p.value,
  t.test(groupe3, groupe5, var.equal = F)$p.value
)

# Ajuster les p-valeurs pour les comparaisons multiples (Bonferroni)
p_values_adjusted <- p.adjust(p_values, method = "bonferroni")

# Afficher les résultats ajustés
p_values_adjusted

#[1] 0.048103725 0.003579952 0.435058880 0.052687767 1.000000000 1.000000000 0.524709947 1.000000000 1.000000000
#Difficile à interprêter, il semble avoir une différence significative entre 1 et 2 et entre 1 et 3, mais presque pas
#significative entre 1 et 5, alors que la moyenne et la médiane sont toutes 2 bien inférieur au groupe 1

##########################
# variable walc


psych::describeBy(mG3, group = mWalc)

#Les variances semblent différent

ggstatsplot::ggbetweenstats(
  data = mathematique, x = Walc, y = G3,
  title = "La note finale en mathématique en fonction de la consommation d'alcool la fin de semaine",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

#Les variance semblent finalement très proche
#on vérifie la normalité

qqnorm(mG3[mWalc == 1])
qqline(mG3[mWalc == 1], col = "red")
qqnorm(mG3[mWalc == 2])
qqline(mG3[mWalc == 2], col = "red")
qqnorm(mG3[mWalc == 3])
qqline(mG3[mWalc == 3], col = "red")
qqnorm(mG3[mWalc == 4])
qqline(mG3[mWalc == 4], col = "red")
qqnorm(mG3[mWalc == 5])
qqline(mG3[mWalc == 5], col = "red")

#les données suivent une loi normale,

groupe1 <- mG3[mWalc == 1]
groupe2 <- mG3[mWalc == 2]
groupe3 <- mG3[mWalc == 3]
groupe4 <- mG3[mWalc == 4]
groupe5 <- mG3[mWalc == 5]


p_values <- c(
  t.test(groupe1, groupe2, var.equal = T)$p.value,
  t.test(groupe1, groupe3, var.equal = T)$p.value,
  t.test(groupe1, groupe4, var.equal = T)$p.value,
  t.test(groupe1, groupe5, var.equal = T)$p.value,
  t.test(groupe2, groupe3, var.equal = T)$p.value,
  t.test(groupe2, groupe4, var.equal = T)$p.value,
  t.test(groupe2, groupe5, var.equal = T)$p.value,
  t.test(groupe3, groupe4, var.equal = T)$p.value,
  t.test(groupe3, groupe5, var.equal = T)$p.value
)

# Ajuster les p-valeurs pour les comparaisons multiples (Bonferroni)
p_values_adjusted <- p.adjust(p_values, method = "bonferroni")

# Afficher les résultats ajustés
p_values_adjusted


#[1] 1.00000000 0.27039427 0.01628743 0.77862114 1.00000000 0.13375697 1.00000000 1.00000000 1.00000000
#Il y a une différence entre les groupe, certain entre 1 et 3

#-----------------------
#portugais

psych::describeBy(pG3, group = pWalc)

#Les variances semblent proche

ggstatsplot::ggbetweenstats(
  data = portugais, x = Walc, y = G3,
  title = "La note finale en en portugais en fonction de la consommation d'alcool la fin de semaine",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)


qqnorm(pG3[pWalc == 1])
qqline(pG3[pWalc == 1], col = "red")
qqnorm(pG3[pWalc == 2])
qqline(pG3[pWalc == 2], col = "red")
qqnorm(pG3[pWalc == 3])
qqline(pG3[pWalc == 3], col = "red")
qqnorm(pG3[pWalc == 4])
qqline(pG3[pWalc == 4], col = "red")
qqnorm(pG3[pWalc == 5])
qqline(pG3[pWalc == 5], col = "red")

#les données suivent une loi normale

groupe1 <- pG3[pWalc == 1]
groupe2 <- pG3[pWalc == 2]
groupe3 <- pG3[pWalc == 3]
groupe4 <- pG3[pWalc == 4]
groupe5 <- pG3[pWalc == 5]


p_values <- c(
  t.test(groupe1, groupe2, var.equal = T)$p.value,
  t.test(groupe1, groupe3, var.equal = T)$p.value,
  t.test(groupe1, groupe4, var.equal = T)$p.value,
  t.test(groupe1, groupe5, var.equal = T)$p.value,
  t.test(groupe2, groupe3, var.equal = T)$p.value,
  t.test(groupe2, groupe4, var.equal = T)$p.value,
  t.test(groupe2, groupe5, var.equal = T)$p.value,
  t.test(groupe3, groupe4, var.equal = T)$p.value,
  t.test(groupe3, groupe5, var.equal = T)$p.value
)

# Ajuster les p-valeurs pour les comparaisons multiples (Bonferroni)
p_values_adjusted <- p.adjust(p_values, method = "bonferroni")

# Afficher les résultats ajustés
p_values_adjusted

#1.0000000000 1.0000000000 0.0007616568 0.0058612511 1.0000000000 0.0127945398 0.0401602842 0.0421373683 0.0802960388
#au moins un des groupe est différent, le groupe 1 et (1,4) et (1,5). Boire de l'alcool les fds c'est pas bon pour le portugais


