#analyse brouillon



#4 niveaux
mTravelTime <- mathematique$traveltime
pTravelTime <- portugais$traveltime

#4 niveaux
mStudyTime <- mathematique$studytime
pStudyTime <- portugais$studytime

#2 niveaux
mSchoolSup <- mathematique$schoolsup
pSchoolSup <- portugais$schoolsup

#2 niveaux
mFamSup <- mathematique$famsup
pFamSup <- portugais$famsup

#2 niveaux
mPaid <- mathematique$paid
pPaid <- portugais$paid

#2 niveaux
mInternet <- mathematique$internet
pInternet <- portugais$internet

#5 niveaux
mFreeTime <- mathematique$freetime
pFreeTime <- portugais$freetime

#5 niveaux
mDalc <- mathematique$Dalc
PDalc <- portugais$Dalc

#5 niveaux
mWalc <- mathematique$Walc
pWalc <- portugais$Walc

#Donc un total de 9 variables (X_i)

mG3 <- mathematique$G3
pG3 <- portugais$G3

#La question. On veut savoir quel variable sont
#importantes, corrélé, et déterminer quel niveau
#pour que dans la 2e partie on tente de déterminer
#de combien on peut augmenter la note G3 pour quel matière
#dans une campagne du ministère de l'éducation
#et peut être savoir si l'on combine plusieurs facteurs



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





###################









ggstatsplot::ggbetweenstats(
  data = mathematique, x = Dalc, y = G3,
  title = "Hauteur ~ Hemisphere",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
)

ggstatsplot::ggbetweenstats(
  data = mathematique, x = Walc, y = G3,
  title = "Hauteur ~ Hemisphere",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
)

ggstatsplot::ggbetweenstats(
  data = portugais, x = Dalc, y = G3,
  title = "Hauteur ~ Hemisphere",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
)

ggstatsplot::ggbetweenstats(
  data = portugais, x = Walc, y = G3,
  title = "Hauteur ~ Hemisphere",
  mean.ci = TRUE,
  type="p",
  var.equal = F,
  bf.message = FALSE,
)

psych::describeBy(portugais$G3, group = portugais$Dalc)
psych::describeBy(portugais$G3, group = portugais$Walc)