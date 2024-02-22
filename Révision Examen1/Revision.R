library(Sleuth3)
data("case0101")
head(case0101)

summary(case0101)
attach(case0101)
View(case0101)

tapply(Score, Treatment, summary)

library(psych)
psych::describeBy(Score, group = Treatment)

t.test(Score ~ Treatment)
?t.test

t.test(Score ~ Treatment, var.equal = T)
####################

mExt <- mean(Score[Treatment == "Extrinsic"])
mInt <- mean(Score[Treatment == "Intrinsic"])
(test.stat1 <- mExt -mInt)
###########################

attach(case0102)
View(case0102)

(stat.test2 <- mean(Salary[Sex == "Male"])- mean(Salary[Sex=="Female"]))
set.seed(2023)
n <- length(Salary)
nPerm <- 1000000
Diff.Obs <- rep(NA, nPerm)
SalairePermute <- rep(NA,n)

for(i in 1:nPerm){
  SalairePermute <- sample(Salary, size = n, replace = F)
  Diff.Obs[i] <- mean(SalairePermute[Sex == "Male"]) - mean(SalairePermute[Sex=="Female"])
}

hist(Diff.Obs, col = "blue", main="Distribution de la différence (..)")
abline(v=stat.test2, col="red")
abline(v=-stat.test2, col="red")

(sum(Diff.Obs <= -stat.test2)+ sum(Diff.Obs >= stat.test2))/nPerm

######################################

t.test(Unaffected,Affected,paired=TRUE,alternative='two.sided')



########
library(Sleuth3)
library(psych)
data("case0301")
attach(case0301)
head(case0301,n=5)
View(case0301)
summary(case0301)
psych::describeBy(Rainfall , group=Treatment)

ggstatsplot::ggbetweenstats(
data = case0301, x = Treatment, y = Rainfall,
title = "Rainfall ~ Treatment",
mean.ci = TRUE,
type="p",
var.equal = TRUE,
bf.message = FALSE,
)

case0301$logRainfall <- log(Rainfall)
View(case0301)
attach(case0301)
ggstatsplot::ggbetweenstats(
  data = case0301, x = Treatment, y = logRainfall,
  title = "Rainfall ~ Treatment",
  mean.ci = TRUE,
  type="p",
  var.equal = TRUE,
  bf.message = FALSE,
)

psych::describeBy(logRainfall , group=Treatment)

###############




