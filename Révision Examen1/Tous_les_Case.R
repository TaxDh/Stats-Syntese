library(Sleuth3)
data("case0101")
data("case0102")
data("case0201")
data("case0202")
data("case0301")
data("case0302")
data("case0401")
data("case0402")
data("case0502")
data("case0501")
data("case0601")
data("case0602")
data("case0701")
data("case0702")
data("case0801")
data("case0802")
data("case0901")
data("case0902")

View(case0101)
View(case0102)
View(case0201)
View(case0202)
View(case0301)
View(case0302)
View(case0401)
View(case0402)
View(case0501)
View(case0502)
View(case0601)
View(case0602)
View(case0701)
View(case0702)

dim(case0501)
summary(case0501)
install.packages("psych")
library(psych)
psych::describeBy(case0501$Lifetime, group = case0501$Diet)
psych::describe(case0501$Lifetime)
attach(case0501)

case0501
levels(Diet)

install.packages("ggstatsplot")
head(case0501)
ggstatsplot::ggbetweenstats(
  data = case0501, x = Diet, y = Lifetime,
  title = "Lifetime ~ Diet",
  mean.ci = TRUE,
  type="p",
  var.equal = TRUE,
  bf.message = FALSE,
)


