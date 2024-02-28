mathematique <- read.table("student-mat.csv", header = T, sep = ",")
portugais <- read.table("student-por.csv", header = T, sep = ",")
head(mathematique)

#variables pertinentes:
#-traveltime
#-studytime
#-failures
#-schoolsup
#-famsup
#-paid
#-internet (peut être pas sure)
#-freetime (probablement moins de devoir?)
#-Dalc
#-Walc
#-absences

mTravelTime <- mathematique$traveltime
pTravelTime <- portugais$traveltime

mStudyTime <- mathematique$studytime
pStudyTime <- portugais$studytime

mFailures <- mathematique$failures
pFailures <- portugais$failures

mSchoolSup <- mathematique$schoolsup
pSchoolSup <- portugais$schoolsup

mFamSup <- mathematique$famsup
pFamSup <- portugais$famsup

mPaid <- mathematique$paid
pPaid <- portugais$paid

mInternet <- mathematique$internet
pInternet <- portugais$internet

mFreeTime <- mathematique$freetime
pFreeTime <- portugais$freetime

mDalc <- mathematique$Dalc
PDalc <- portugais$Dalc

mWalc <- mathematique$Walc
pWalc <- portugais$Walc

mAbsences <- mathematique$absences
pAbsences <- portugais$absences

