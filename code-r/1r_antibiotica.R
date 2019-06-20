#####################
# antibiotics
#####################

library(lme4)

##---- set working directory ----
rm(list=ls())

normalizePath(readClipboard(),winslash = "/")
setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_20190620/code-r")

##---- read in data from excel file ----
dat <- read.table("../data/1r_enzyme_activity.txt", header=TRUE)

str(dat)
# 'data.frame':	16 obs. of  2 variables:
# $ batch   : int  1 1 2 2 3 3 4 4 5 5 ...
# $ activity: int  40 42 33 34 46 47 55 52 63 59 ...

dat$batch <- as.factor(dat$batch)

one.way.fix  <- lm(activity ~ 1 + batch, data=dat)
summary(one.way.fix)

anova(one.way.fix)

one.way.ran  <- lmer(activity ~ 1 + (1|batch), data=dat)
summary(one.way.ran)
