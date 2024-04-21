#Test

donnees <- read.table("student-mat_por.csv", sep = ",", header = TRUE)


which(donnees$G3.x == 0)
donnees <- donnees[donnees$G3.x > 0, ]
which(donnees$G3.x == 0)

which(donnees$G3.y == 0)
donnees <- donnees[donnees$G3.y > 0, ]
which(donnees$G3.y == 0)


# Pour la variable 'traveltime' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = traveltime.x, y = G3.x,
  title = "La note finale en mathématique en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'traveltime' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = traveltime.y, y = G3.y,
  title = "La note finale en portugais en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'studytime' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = studytime.x, y = G3.x,
  title = "La note finale en mathématique en fonction du temps d'étude",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)
# Significatif!2


# Pour la variable 'studytime' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = studytime.y, y = G3.y,
  title = "La note finale en portugais en fonction du temps d'étude",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)
# Significatif!2

# Pour la variable 'schoolsup' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = schoolsup.x, y = G3.x,
  title = "La note finale en mathématique en fonction du support scolaire",
  mean.ci = TRUE,
  type="p",
  var.equal = T,
  bf.message = FALSE
)
# Significatif!2

# Pour la variable 'schoolsup' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = schoolsup.y, y = G3.y,
  title = "La note finale en portugais en fonction du support scolaire",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)
# Significatif!2

# Pour la variable 'famsup' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = famsup.x, y = G3.x,
  title = "La note finale en mathématique en fonction du support familial",
  mean.ci = TRUE,
  type="p",
  var.equal = T,
  bf.message = FALSE
)
# Pas significatif


# Pour la variable 'famsup' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = famsup.y, y = G3.y,
  title = "La note finale en portugais en fonction du support familial",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'paid' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = paid.x, y = G3.x,
  title = "La note finale en mathématique en fonction du support payé",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)
# Significatif! p hacking p = 0.05

# Pour la variable 'paid' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = paid.y, y = G3.y,
  title = "La note finale en portugais en fonction du support payé",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)
# Significatif!2

# Pour la variable 'internet' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = internet, y = G3.x,
  title = "La note finale en mathématique en fonction de l'accès à Internet",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)
# Significatif!2 p = 0.05

# Pour la variable 'internet' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = internet, y = G3.y,
  title = "La note finale en portugais en fonction de l'accès à Internet",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'freetime' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = freetime.x, y = G3.x,
  title = "La note finale en mathématique en fonction du temps libre",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)



# Pour la variable 'freetime' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = freetime.y, y = G3.y,
  title = "La note finale en portugais en fonction du temps libre",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)
# Significatif!2 p = 0.05

# Pour la variable 'Dalc' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = Dalc.x, y = G3.x,
  title = "La note finale en mathématique en fonction de la consommation d'alcool en semaine",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'Dalc' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = Dalc.y, y = G3.y,
  title = "La note finale en portugais en fonction de la consommation d'alcool en semaine",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)
# Significatif!2

# Pour la variable 'Walc' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = Walc.x, y = G3.x,
  title = "La note finale en mathématique en fonction de la consommation d'alcool le weekend",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)
# Significatif!2

# Pour la variable 'Walc' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = Walc.y, y = G3.y,
  title = "La note finale en portugais en fonction de la consommation d'alcool le weekend",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)
# Significatif!2


donnees$diff.x <- donnees$G3.x - donnees$G1.x
donnees$diff.y <- donnees$G3.y - donnees$G1.y


##################################################################################
# Maintenant on le fait pour la différence entre G3 et G1

# Pour la variable 'traveltime' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = traveltime.x, y = diff.x,
  title = "La différence de la note finale en mathématique en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'traveltime' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = traveltime.y, y = diff.y,
  title = "La différence de la note finale en portugais en fonction du temps de voyage",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'studytime' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = studytime.x, y = diff.x,
  title = "La différence de la note finale en mathématique en fonction du temps d'étude",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'studytime' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = studytime.y, y = diff.y,
  title = "La différence de la note finale en portugais en fonction du temps d'étude",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'schoolsup' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = schoolsup.x, y = diff.x,
  title = "La différence de la note finale en mathématique en fonction du support scolaire",
  mean.ci = TRUE,
  type="p",
  var.equal = TRUE,
  bf.message = FALSE
)
# Significatif!

# Pour la variable 'schoolsup' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = schoolsup.y, y = diff.y,
  title = "La différence de la note finale en portugais en fonction du support scolaire",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'famsup' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = famsup.x, y = diff.x,
  title = "La différence de la note finale en mathématique en fonction du support familial",
  mean.ci = TRUE,
  type="p",
  var.equal = TRUE,
  bf.message = FALSE
)

# Pour la variable 'famsup' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = famsup.y, y = diff.y,
  title = "La différence de la note finale en portugais en fonction du support familial",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'paid' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = paid.x, y = diff.x,
  title = "La différence de la note finale en mathématique en fonction du support payé",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)
# Significatif!


# Pour la variable 'paid' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = paid.y, y = diff.y,
  title = "La différence de la note finale en portugais en fonction du support payé",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'internet' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = internet, y = diff.x,
  title = "La différence de la note finale en mathématique en fonction de l'accès à Internet",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'internet' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = internet, y = diff.y,
  title = "La différence de la note finale en portugais en fonction de l'accès à Internet",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)


# Pour la variable 'Dalc' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = Dalc.x, y = diff.x,
  title = "La différence de la note finale en mathématique en fonction de la consommation d'alcool en semaine",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'Dalc' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = Dalc.y, y = diff.y,
  title = "La différence de la note finale en portugais en fonction de la consommation d'alcool en semaine",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

# Pour la variable 'Walc' en mathématique
ggstatsplot::ggbetweenstats(
  data = donnees, x = Walc.x, y = diff.x,
  title = "La différence de la note finale en mathématique en fonction de la consommation d'alcool le weekend",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)
# Significatif!2

# Pour la variable 'Walc' en portugais
ggstatsplot::ggbetweenstats(
  data = donnees, x = Walc.y, y = diff.y,
  title = "La différence de la note finale en portugais en fonction de la consommation d'alcool le weekend",
  mean.ci = TRUE,
  type="p",
  var.equal = FALSE,
  bf.message = FALSE
)

