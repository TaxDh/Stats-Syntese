#ma réponse moins pire la ptite bute pire strongly skewed réponse page 209, le pire est long tailed
#2e question, première colonne n1 = 10, n2 = 40

library(Sleuth3)
data("case0301")
attach(case0301)

ggstatsplot::ggbetweenstats(
  data = case0301, x = Treatment, y = Rainfall,
  title = 'Rainfall versus Traitement',
  var.equal = T,
  bf.message = F, 
)
# apres le graphique, on trouve que les variance sont trop inégales, donc on fait une transformation logarithmique

case0301$logRainfall <- log(Rainfall)

ggstatsplot::ggbetweenstats(
  data = case0301, x = Treatment, y = logRainfall,
  title = 'Rainfall versus Traitement',
  var.equal = T,
  bf.message = F, 
)

#on refait le premier graphique en supposant que les variances ne sont pas égales
ggstatsplot::ggbetweenstats(
  data = case0301, x = Treatment, y = Rainfall,
  title = 'Rainfall versus Traitement',
  var.equal = F,
  bf.message = F, 
)
#le p N'est pas mieux

#page 202, positive skew => asymétrie à droite (le droit ou gauche, c'est le trou)

#page 209, quand on simule ces distribution avec un certain n, à 95% on obtient le pourcentage écrit (98.3 n=5 long tailed)

#proc diff en sas pour vérifier la différence entre 2 séries de données (saisies de données)

#case0302

data(case0302)
attach(case0302)

ggstatsplot::ggbetweenstats(
  data = case0302, x = Veteran, y = Dioxin,
  title = 'Distribution du niveau de dioxine chez les vétérans',
  var.equal = T,
  bf.message = F, 
)

#on veut enlever les 2 valeures extrêmes
which(case0302$Dioxin > 20)
case0302_2 <- case0302[-c(645,646),]
which(case0302_2$Dioxin > 20)

ggstatsplot::ggbetweenstats(
  data = case0302_2, x = Veteran, y = Dioxin,
  title = 'Distribution du niveau de dioxine chez les vétérans',
  var.equal = T,
  bf.message = F, 
)


#donc a l'examen, on fait les boxplot (ou les violin), on commente la distribution des données, la variance
#la moyenne, l'asymétrie, les valeurs aberrantes et on change les données (si besoin)
#on vérifie que le changement est bon, s'il est on conclue pt avec un test.t

#page 241, on calcul la différence de moyenne sur les log et on calcul l'effet multiplicatif sur les log
#et on ....

res <- t.test(Rainfall ~ Treatment, var.equal = T)
res

res2 <- t.test(case0301$logRainfall ~ Treatment, var.equal = T)
res2

diff <- res2$estimate[1] - res2$estimate[2]
diff

(estime <- exp(diff))

ci <- t.test(case0301$logRainfall ~Treatment, var.equal = T)$conf.int
ci
exp(ci)
