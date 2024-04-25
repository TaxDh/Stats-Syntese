donnees <- read.table("student-mat_por.csv", sep = ",", header = TRUE)

# G3.x veut dire "note du 3e trimestre pour le cours de mathématique
# G3.y veut dire "note du 3e trimestre pour le cours de portugais

# On enlève toutes les données qui ont G3 à 0%

which(donnees$G3.x == 0)
donnees <- donnees[donnees$G3.x > 0, ]
which(donnees$G3.x == 0)

which(donnees$G3.y == 0)
donnees <- donnees[donnees$G3.y > 0, ]
which(donnees$G3.y == 0)

#On créé une variable différence pour évaluer l'évolution de la note par rapport
#à une variable.

donnees$diff.x <- donnees$G3.x - donnees$G1.x
donnees$diff.y <- donnees$G3.y - donnees$G1.y


################################################
# Exemple de code pour les statistiques descriptives
summary(donnees$G3.x)
summary(donnees$G3.y)
summary(donnees$diff.x)
summary(donnees$diff.y)

# Histogrammes
hist(donnees$G3.x, main="Distribution des notes G3 en mathématiques", xlab="Notes")
hist(donnees$G3.y, main="Distribution des notes G3 en portugais", xlab="Notes")
###################################################

####################################################
cor(donnees$G3.x, donnees$G1.x)
cor(donnees$G3.y, donnees$G1.y)
####################################################

# ANOVA pour une variable catégorielle affectant G3.x
anova_result <- aov(G3.x ~ sex, data = donnees)
summary(anova_result)

# ANOVA à deux facteurs avec interaction
anova_two_way <- aov(G3.x ~ sex + address + sex:address, data = donnees)
summary(anova_two_way)
