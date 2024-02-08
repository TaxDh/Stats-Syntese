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
