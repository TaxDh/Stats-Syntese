library(car)
library(Sleuth3)
attach(case0102)
case0102


mmale <- median(Salary[Sex == "Male"])
mfemale <- median(Salary[Sex == "Female"])
Salaire <- rep(NA, length((length(Salary))))

#Solution 1
for(i in 1:length(Salary)){
  if(Sex[i] == "Male"){
    Salaire[i] <- abs(Salary[i] - mmale)
  }
  else{
    Salaire[i] <- abs(Salary[i]-mfemale)
  }
}

t.test(Salaire ~ Sex, var.equal = T)

#Solution 2

Salaire <- ifelse(Sex == "Male", abs(Salary - mmale), abs(Salary -mfemale))#à connaitre, c'est cool cette affaire là
t.test(Salaire ~ Sex, var.equal = T)



######################################################################################################
#Les souris page 363


attach(case0501)
View(case0501)
ggstatsplot::ggbetweenstats(
  data = case0501, x = Diet, y = Lifetime,
  title = "Diet", 
  bf.message = F,
  var.equal = T,
)

#Density plots
require(mosaic)

densityplot(~Lifetime, groups = Diet, auto.key = T, data = case0501)

densityplot(~Lifetime | Diet, data = case0501, xlab = "Diet", layout = c(1,6))

################################
#page 374


ggstatsplot::ggbetweenstats(
  data = case0502, x = Judge, y = Percent,
  title = "Judge Spock", 
  bf.message = F,
  var.equal = T,
)
#j'avais raison, la p-value est bien 10^{-5}. WAHOUUU!

















































