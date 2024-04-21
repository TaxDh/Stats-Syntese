d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)

colnames(d1)
colnames(d2)












d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

write.csv(d3, file = "D:/Documents/Trimestre Actuel/STT3200 - Synthèse/Stats-Syntese/Projet/sudent-mat_por.csv", row.names = FALSE)

print(nrow(d3)) # 382 students
View(d3)
head(d3)


# Étape 1 : Relire le fichier CSV
d3_read <- read.table("student-mat_por.csv" ,sep=",", header = TRUE)

# Étape 2 : Vérifier les dimensions
dim(d3_read)
dim(d3)

# Étape 3 : Inspecter les premières et dernières lignes
head(d3_read)
tail(d3_read)

# Étape 4 : Vérifier les noms des colonnes
colnames(d3_read)
colnames(d3)

View(d3_read)



which(d1$G3 == 1)
which(d2$G3 == 1)

which(d3_read$G3.x == 1)
which(d3_read$G3.y == 1)

d2[173, ]
d3_read[239,]
