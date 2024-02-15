#inkscape pour modifier des pdf utile pour le rapport

#on fait le truc des joint torique de la NASA page 266

library(Sleuth3)
data("case0401")
attach(case0401)
head(case0401)
dim(case0401)


t.test(Incidents ~ Launch, var.equal = T)
# p-value 0.0007929. Très significatif, mais ecart-type différent donc t.test pas tres bon

#Test de permutation
nperm <- 100000
statistiquet <- rep(NA, nperm)
for(i in 1:nperm){
  statistiquet[i] <- t.test(Incidents ~ sample(Launch, size = length(Launch), replace = F), var.equal = T)$statistic
}
hist(statistiquet, col = "gold", nclass = 100)
box()
abline(v=3.8876,col="red")
mean(statistiquet > 3.8876)

# t = 3.8876
# permutation p-value: 0.0096

#note t = 3.8876 est une statistique t, pas un test t.


#modification - bon il y a des bogues, faudra que je regarde plus tard
mastat <- t.test(Incidents ~ Launch, var.equal = T)
diff <- mastat[1] - mastat[2]
nperm <- 10000
statistiquet <- rep(NA, nperm)
diffmoyennes <- rep(NA, nperm)
for(i in 1:nperm){
  statistiquet <- t.test(Incidents ~ sample(Launch, size = length(Launch), replace = F), var.equal = T)$estimate
  diffmoyennes[i] <- statistiquet[1] - statistiquet[2]
}
hist(diffmoyennes, col = "gold", nclass = 100)
box()
abline(v=3.8876,col="red")
mean(diffmoyennes > 3.8876)

###########################################
#page 289
####################
data("case0402")
head(case0402)
attach(case0402)
mastat <- wilcox.test(Time ~ Treatment)$statistic

nperm <- 1000
statistiqueW <- rep(NA, nperm)
for(i in 1:nperm){
  statistiqueW[i] <- wilcox.test(Time ~ sample(Treatment, size = length(Treatment), replace = F))$statistic
  print(i)
}
hist(statistiqueW, col = "gold", nclass = 100)
box()
abline(v=mastat,col="red")
mean(statistiqueW > mastat)

#bon encore bogué, ou pt pas, je ne suis pas sure, disons que la p-value m'apparait bizarre
###########################################################################

##########################################################################
#page 300

delta <- seq(40,170,5)
valeurp <- rep(NA, length(delta))
for(i in 1:length(delta)){
  Temps <- case0402$Time
  Temps[Treatment == "Modified"] <- Temps + delta[i]
  valeurp[i] <- wilcox.test(Temps ~ Treatment, exact = T)$p.value
}
plot(delta, valeurp, type = 'b', pch=16)
abline(h=0.05, col="red")
#j'ai moins de données, mais ça marche








