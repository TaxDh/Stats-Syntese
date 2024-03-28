library(Sleuth3): attach(case0502)
#Judge <- factor(Judge, levels = c("Spock's", "A", "B", "C", "D","E","F"))# on avait pas besoin de mettre en facteur


n <- length(Percent)

res_red <- Percent - mean(Percent) # Residus modele reduit
lanova <- aov(Percent ~ Judge, data = case0502)
res_full <- resid(lanova) # Residus modele complet

par(mfrow = c(2,1), mar=c(3,3,1,1))
plot(1:n, res_red, col = rainbow(7)[as.numeric(Judge)], pch = 16, ylim = c(-25,25))
abline(h=0)

segments(1:n, res_red, 1:n, 0, col=rainbow(7)[as.numeric(Judge)] )# pour faire jolie

plot(1:n, res_full, col = rainbow(7)[as.numeric(Judge)], pch = 16, ylim = c(-25,25))
abline(h=0)

segments(1:n, res_red, 1:n, 0, col=rainbow(7)[as.numeric(Judge)] )# pour faire jolie

#Petit bogue par rapport au segment du 2e graphique, je ne sais pas pourquoi, c'est R...

anova(lm(Percent ~ Judge, data = case0502))
anova(lm(Percent ~ as.factor(Judge)))
aov(lm(Percent ~ Judge, data = case0502))


with(subset(case0502, Judge != "Spock's"),
anova(lm(Percent ~ as.factor(Judge))))



#page 412


