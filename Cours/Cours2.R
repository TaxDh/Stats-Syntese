library(Sleuth3)
data("case0101") ; attach(case0101)
mExt <- mean(Score[Treatment == "Extrinsic"])
mInt <- mean(Score[Treatment == "Intrinsic"])
test.stat1 <- mExt -mInt


set.seed(2024)
n <- length(Score) ; nPerm <- 1000000
RediffObs <- rep(0,nPerm) ; ReEchantillonT <- rep(NA, n)
#On va randomiser le traitement et on va le mettre dans le vecteur ReEchantillon
for(i in 1:nPerm){
  ReEchantillonT <- sample(Treatment, size = n, replace = FALSE)
  RediffObs[i] <- mean(Score[ReEchantillonT == "Extrinsic"]) - mean(Score[ReEchantillonT == "Intrinsic"])
}

hist(RediffObs, col="dodgerblue3", main= "Dist de la diffé")
box()
#abline (v = ...) veut dire ajouter une ligne verticale
abline(v=test.stat1, col="red")
abline(v=-test.stat1,col="red")
#Test bilateral
((sum(RediffObs <= test.stat1) + sum(RediffObs >= -test.stat1))/nPerm)
#Test unilateral je crois
((sum(RediffObs <= test.stat1))/nPerm)

#page 53: test de permutations
#Exercice 2

data("case0102")
attach(case0102)

mExt <- mean(Salary[Sex == "Male"])
mInt <- mean(Salary[Sex == "Female"])
stat.test <- mExt - mInt

set.seed(1234)
n <- length(Salary)
nPerm <- 100000
RediffObs <- rep(0,nPerm)
SalairePermute <- rep(NA,n)
for(i in 1:nPerm){
  SalairePermute <- sample(Salary, size=n, replace = FALSE)  
  RediffObs[i] <- mean(SalairePermute[Sex == "Male"]) - mean(SalairePermute[Sex == "Female"])
}

hist(RediffObs, col="dodgerblue3", main= "Dist de la diffé")
box()
#Les lignes sont trop loin
abline(v=stat.test, col="red")
abline(v=stat.test,col="red")
#Test bilateral
((sum(RediffObs >= stat.test) + sum(RediffObs <= -stat.test))/nPerm)
#Test unilateral je crois
((sum(RediffObs >= stat.test))/nPerm)

stat.test#les lignes rouges sont trop loin pour apparaitre sur le graphique
#etude de cas de la ssc, donnees
