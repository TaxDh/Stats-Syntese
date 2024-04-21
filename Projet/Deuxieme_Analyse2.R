set.seed(2024)

donnees <- read.table("student-mat_por.csv", sep = ",", header = TRUE)

# G3.x veut dire "note du 3e trimestre pour le cours de mathématique
# G3.y veut dire "note du 3e trimestre pour le cours de portugais

# On enlève toutes les données qui ont G3 à 0%

library(car)
library(MASS)
install.packages("lmtest")
library(lmtest)


which(donnees$G3.x == 0)
donnees <- donnees[donnees$G3.x > 0, ]
which(donnees$G3.x == 0)

which(donnees$G3.y == 0)
donnees <- donnees[donnees$G3.y > 0, ]
which(donnees$G3.y == 0)

# à cause de plusieurs analyse, on change le "yes", "no" en 1 et 0 respectivement.
donnees$schoolsup.x <- ifelse(donnees$schoolsup.x == "yes", 1, 0)
donnees$schoolsup.y <- ifelse(donnees$schoolsup.y == "yes", 1, 0)

donnees$famsup.x <- ifelse(donnees$famsup.x == "yes", 1, 0)
donnees$famsup.y <- ifelse(donnees$famsup.y == "yes", 1, 0)

donnees$paid.x <- ifelse(donnees$paid.x == "yes", 1, 0)
donnees$paid.y <- ifelse(donnees$paid.y == "yes", 1, 0)

donnees$internet <- ifelse(donnees$internet == "yes", 1, 0)


#On créé une variable différence pour évaluer l'évolution de la note par rapport
#à une variable.

donnees$diff.x <- donnees$G3.x - donnees$G1.x
donnees$diff.y <- donnees$G3.y - donnees$G1.y

donnees_sauvegarde <- donnees # une sauvegarde des données

# On créé notre premier modèle d'anova
model1 <- lm(G3.x ~ traveltime.x + studytime.x + schoolsup.x + famsup.x + paid.x + internet + Dalc.x + Walc.x, data = donnees)

# Vérification de la linéarité et homoscédasticité
plot(model1, which = 1)
# C'est à peu près linéaire et ça respecte l'homooscédasticité

# vérifie la normalité des résidus
qqPlot(model1$residuals,pch=20)
# Les résidus suivent la loi normale.

# 2e test pour s'en assurer
shapiro.test(residuals(model1))
# W = 0.99339, p-value = 0.1417, W proche de 1 veut dire proche loi normale
# p-value > 0.05 veut dire qu'on ne peut pas rejeter l'hypothese nulle que les résidus sont normale


# On sait déjà par la première partie que les variances sont inégales

# Recherche de la meilleure transformation Box-Cox
bc_result <- boxcox(model1, lambda = seq(-2, 2, by = 0.1))

# Trouver la valeur de lambda qui maximise la log-vraisemblance
(best_lambda <- bc_result$x[which.max(bc_result$y)])

# Appliquer la transformation Box-Cox à G3.x
donnees$boxcoxG3.x <- (donnees$G3.x^best_lambda - 1) / best_lambda

# On applique avec le modèle avec boxcoxG3.x
model1B <- lm(boxcoxG3.x ~ traveltime.x + studytime.x + schoolsup.x + famsup.x + paid.x + internet + Dalc.x + Walc.x, data = donnees)

# On vérifie la normalité des résidus
qqnorm(residuals(model1B))
qqline(residuals(model1B), col = "red")
# Respecte la normalité

# On doit vérifier la variance des résidus

attach(donnees)
################



plot(model1B, which = 1)
qqPlot(model1B$residuals,pch=20)

par(mfrow=c(2,2))

# Graphiques de dispersion avec lignes de régression
plot(traveltime.x , boxcoxG3.x ); R <- lm(boxcoxG3.x ~ traveltime.x) ; abline(R, col="red3")
plot(studytime.x , boxcoxG3.x ); R <- lm(boxcoxG3.x ~ studytime.x) ; abline(R, col="red3")
plot(schoolsup.x , boxcoxG3.x ); R <- lm(boxcoxG3.x ~ schoolsup.x) ; abline(R, col="red3")
plot(famsup.x , boxcoxG3.x ); R <- lm(boxcoxG3.x ~ famsup.x) ; abline(R, col="red3")

par(mfrow=c(2,2))
# Graphiques de résidus
plot(paid.x , boxcoxG3.x ); R <- lm(boxcoxG3.x ~ paid.x) ; abline(R, col="red3")
plot(internet , boxcoxG3.x ); R <- lm(boxcoxG3.x ~ internet) ; abline(R, col="red3")
plot(Dalc.x , boxcoxG3.x ); R <- lm(boxcoxG3.x ~ Dalc.x) ; abline(R, col="red3")
plot(Walc.x , boxcoxG3.x ); R <- lm(boxcoxG3.x ~ Walc.x) ; abline(R, col="red3")

par(mfrow=c(1,1))

# vérification de l'indépendance des résidus

dwtest(model1B)
# DW = 1.9408, p-value = 0.2859
# Dw proche de 2 veut dire indépendance des résidus, p-valeur > 0.05 on ne rejette pas H0 qui dit que les résidus sont indépendant


Anova(model1B, type="II")


# On tente des modifications au modèle:

model_interactions.x <- lm(boxcoxG3.x ~ (traveltime.x + studytime.x + schoolsup.x + famsup.x + paid.x + internet + Dalc.x + Walc.x)^2, data = donnees)


plot(model_interactions.x, which = 1) #  C'est à peu près linéaire et ça respecte l'homooscédasticité

qqPlot(model_interactions.x$residuals,pch=20) # résidus normal


# vérification de l'indépendance des résidus

dwtest(model_interactions.x) # variance résidus égales
# DW = 2.0031, p-value = 0.4947

Anova(model_interactions.x, type = "II")

# On observe les 2 intéractions, car c'est louche (voir partie 1 projet)

library(ggplot2)

# Interaction entre studytime.x et schoolsup.x
ggplot(donnees, aes(x=studytime.x, y=boxcoxG3.x, color=factor(schoolsup.x))) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, aes(group=schoolsup.x)) +
  labs(title="Interaction entre Study Time et School Support",
       x="Temps d'étude",
       y="Note 3e Trimestre") +
  scale_color_manual(values=c("blue", "red")) +
  theme_minimal()
# Donc même problème qu'au projet partie 1

donnees$famsup.x <- as.factor(donnees$famsup.x)


# Interaction entre studytime.x et famsup.x
ggplot(donnees, aes(x=studytime.x, y=boxcoxG3.x, color=famsup.x)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, aes(group=famsup.x)) +
  labs(title="Interaction entre Family Support et Box-Cox Transformed G3",
       x="Temps d'étude",
       y="Note 3e Trimestre") +
  scale_color_manual(values=c("blue", "red")) +
  theme_minimal()
#celui-ci est gardable a partir du niveau 3 de temps d'étude avec le support familiale il bat le non support
# mais je pense que c'est simplement l'effet du temps d'étude qui est très significatif.


#######################################################################################
# Maintenant on le fait pour diff.x

model2 <- lm(diff.x ~ (traveltime.x + studytime.x + schoolsup.x + famsup.x + paid.x + internet + Dalc.x + Walc.x)^2, data = donnees)


