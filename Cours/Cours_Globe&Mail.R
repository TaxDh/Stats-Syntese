#"data journaliste"

GlobeandMail <- read.csv("GlobeandMail.csv", row.names=1)
dim(GlobeandMail)

View(GlobeandMail)
income <- GlobeandMail$X11.MedianHouseholdIncome2020
