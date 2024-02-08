#lecture du fichier et petite visualisation

Gold <- read.table("FINAL_USO.csv", header = TRUE, sep = ",")
head(Gold)
summary(Gold)
attach(Gold)
better_gold <- Gold[,-1]

head(better_gold)
modele_Gold <- lm(Close ~., data = better_gold)
modele_Gold
anova(modele_Gold)
#anova(lm(Close ~., data = Gold))
names(Gold)

plot(Gold)
boxplot(Close ~ GDX_Close, col = "dodgerblue3")
