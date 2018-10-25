library(survminer)
library(survival)
library(coin)

rawData <- read.delim('./data.tsv', stringsAsFactors = FALSE)
str(rawData)

dat <- within(rawData, {
  dBirth <- as.Date(dBirth, format = '%m/%d/%Y')
  dDiagnosis <- as.Date(dDiagnosis, format = '%m/%d/%Y')
  ageOver80 <- factor(age > 80)
  alive <- alive == 1
  dDeath <- as.Date(Date.of.death, format = '%m/%d/%Y')
  timeToDeath <- ifelse(!is.na(dDeath), (dDeath - dDiagnosis) / 365, followup)
  death <- alive != 1
  terapyChar <- ifelse(terapy == 1, 'Chemo', 'Mixed')
  terapyF <- factor(terapyChar)
  time <- ifelse(is.na(timeToProgression), followup, timeToProgression)
  time <- time / 12
  event <- !is.na(timeToProgression)
})

summary(lm(age ~ terapy, data = dat))


fisher.test(dat$alive, dat$terapyF)
fisher.test(dat$event, dat$terapyF)

xtabs(~event + terapyF, data = dat)

summary(dat$age)
summary(dat$time)
summary(dat$timeToDeath)
str(dat)

## Kaplan Meier

survdiff(Surv(timeToDeath, death) ~ terapyF, data = dat)

logrank_test(Surv(time, event) ~ terapyF | ageOver80, data = dat, distribution = approximate(B = 10000))

fit <- coxph(Surv(time, event) ~ terapyF + I(age > 80), data = dat)
summary(fit)
