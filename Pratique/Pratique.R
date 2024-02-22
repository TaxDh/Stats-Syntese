#install.packages('Sleuth3")?
library(Sleuth3)

#page 13 des notes
head(case0101, n=5)
summary(case0101)

attach(case0101)
tapply(Score, Treatment, summary)#je ne sais pas trop à quoi ça sert

library(psych)
psych::describeBy(Score, group=Treatment)

boxplot(Score~Treatment)
#boxplot(Score~Treatment, col = "dodgerblue3")

#liste de couleur R-Colors-a4.pdf
boxplot(Score~Treatment, col = "darkgoldenrod2")

stripchart(Score ~ Treatment, vertical = T, methode = "jitter", add = T, pch = 20, cex = 2.2, col = "red4")

means <- tapply(Score, Treatment, mean)
means

points(means, col="dodgerblue4", pch=1, cex= 2.4, lwd=4)


t.test(Score ~ Treatment)

t.test(Score ~ Treatment, var.equal = T)

#page 41
attach(case0101)
head(case0101)

mExt <- mean(Score[Treatment == "Extrinsic"])
mExt
mInt <- mean(Score[Treatment == "Intrinsic"])
mInt

(test.stat1 <- mExt-mInt)

set.seed(2022)
n <- length(Score)
nPerm <- 1000000
Rediff0bs <- rep(0,nPerm)
ReEchantillonT <- rep(NA,n)

for(i in 1:nPerm){
  ReEchantillonT <- sample(Treatment, size = n, replace = FALSE)
  Rediff0bs[i] <- mean(Score[ReEchantillonT == "Extrinsic"]) - mean(Score[ReEchantillonT == "Intrinsic"])
}

hist(Rediff0bs, col="dodgerblue3", main= "Dist de la diffé")
box()
#abline (v = ...) veut dire ajouter une ligne verticale
abline(v=test.stat1, col="red")
abline(v=-test.stat1,col="red")
#Test bilateral
((sum(Rediff0bs <= test.stat1) + sum(Rediff0bs >= -test.stat1))/nPerm)
#Test unilateral je crois
((sum(Rediff0bs <= test.stat1))/nPerm)





















