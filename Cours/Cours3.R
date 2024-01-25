library(Sleuth3)

data("case0201")
attach(case0201)

ggstatsplot::ggbetweenstats(
    data = case0201, x = Year, y = Depth,
    title = "Profondeur du bec",
    mean.ci = TRUE,
    type = 'p',
    var.equal = TRUE,
    bf.message = FALSE
)
#utiliser jmp est mieux

#pas rapport mais facteur de bayes vs p-value, le facteur de bayes mesure la taille des effets

data("case0202")
attach(case0202)
case0202

#le rapport t, à voir chapitre 2.2
#page 127, rapport t = (x1_barre - x2_barre)/(s/sqrt(n))
data2 = case0202
dim(data2)


#construisons un grand nombre d'intervalle de confiance d'un paramètre mu (dans le cas sigma connu disons),
#et regardons combien d'intervalles contiennent la moyenne de notre population N(10,1). On se souvient que
# l'intervalle pour mu est donné par : x_barre ± z^* sigma/sqrt(n). page 133 (2.2)
#on a skip l'exo


