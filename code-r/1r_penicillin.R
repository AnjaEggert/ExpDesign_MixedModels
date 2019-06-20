#####################
# covariance matrix
#####################

##---- set working directory ----
rm(list=ls())

normalizePath(readClipboard(),winslash = "/")
setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_20190620/code-r")

##---- libraries ----
library(lme4)

##---- load function ----
source("rescov.R")


##---- analyse Penicillin data set from lme4 package ----
dat <-Penicillin
head(dat, 10)

str(dat)
# 'data.frame':	144 obs. of  3 variables:
# $ diameter: num  27 23 26 23 23 21 27 23 26 23 ...
# $ plate   : Factor w/ 24 levels "a","b","c","d",..: 1 1 1 1 1 1 2 2 2 2 ...
# $ sample  : Factor w/ 6 levels "A","B","C","D",..: 1 2 3 4 5 6 1 2 3 4 ...

xtabs(~ plate + sample, dat)

##---- run mixed model: single random effect ----
# ignore the plate level effects
# fit a model with a random intercept only for sample
mod  <- lmer(diameter ~ 1 + (1|sample), data=dat)

summary(mod)

##---- extract covariance matrix ----
# result is block diagonal, with 6 blocks
# each corresponding to one of the samples
# This implies that the repeated measurements within each sample is correlated
# but between samples are not correlated

# The data is re-ordered by sample to improve visualization.
# You generally want the data sorted first by the higher level,
# then within that level, the next highest level, etc.
dat <- dat[order(dat$sample),] 

rc <- rescov(mod, dat)
image(rc)

png("../FIG/CovMatrix_1r.png", width = 10, height = 11, units = 'cm', res = 300, type='cairo')
image(rc)
dev.off()

##---- run mixed model: Cross random effects ----
mod2 <- lmer(diameter ~ 1 + (1 | sample) + (1 | plate), data = dat)

summary(mod2)
dat <- dat[order(dat$plate),] 

rc2 <- rescov(mod2, dat)
image(rc2)

# zoom in
image(rc2[1:24, 1:24])

png("../FIG/CovMatrix_2r_cross_large.png", width = 10, height = 11, units = 'cm', res = 300, type='cairo')
image(rc2)
dev.off()

png("../FIG/CovMatrix_2r_cross_small.png", width = 10, height = 11, units = 'cm', res = 300, type='cairo')
image(rc2[1:24, 1:24])
dev.off()
