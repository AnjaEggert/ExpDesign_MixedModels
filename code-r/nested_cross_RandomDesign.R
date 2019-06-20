#####################
# covariance matrix
#####################

##---- set working directory ----
rm(list=ls())

normalizePath(readClipboard(),winslash = "/")
setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_20190620/code-r")

##---- libraries ----
library(lme4)

##---- analyse Penicillin data set from lme4 package ----
dat <- read.csv("../data/school.csv")

##---- look at data ----
str(dat)
xtabs(~ school + class, dat)

# cross tabulation that every class ID appears in every school
# = crossed random effects
# fully crossed random effects, because every class occurs in every school

##---- random model with nested effects ----
m0 <- lmer(extro ~ open + agree + social + (1 | school/class), data = dat)

summary(m0)

##---- random model with crossed effects ----
m1 <- lmer(extro ~ open + agree + social + (1 | school) + (1 |class), data = dat)

summary(m1)




