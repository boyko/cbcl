library(survminer)
library(survival)

rawData <- read.delim('./data.tsv', stringsAsFactors = FALSE)
str(rawData)

dat <- within(rawData, {
  dBirth <- as.Date(dBirth, format = '%m/%d/%Y')
  dDiagnosis <- as.Date(dDiagnosis, format = '%m/%d/%Y')
  alive <- alive == 1
  dDeath <- as.Date(Date.of.death, format = '%m/%d/%Y')
  terapy <- ifelse(terapy == 1, 'Chemo', 'Mixed')
  eventTime <- ifelse(is.na(timeToProgression), followup, timeToProgression)
  eventTime <- eventTime / 12
  event <- !is.na(timeToProgression)
})

str(dat)
