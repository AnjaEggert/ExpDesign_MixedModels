#####################
# spatial data
#####################

##---- libraries ----
library(nlme)

##---- set working directory ----
rm(list=ls())

normalizePath(readClipboard(),winslash = "/")
setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_20190620/code-r")

##---- read in data ----
dat <- read.csv("../data/spdata_soil.csv")

##---- mixed model ----
# lme() requires a grouping variable
# there is no grouping variable in the data
# create a dummy variable that has the same value for all 75 observations.

dummy <- rep(1, 75) 
dat <- cbind(dat, dummy) 
soil.model <- lme(fixed = thick ~ soil, data = dat, random = ~ 1 | dummy, method = "ML") 
summary(soil.model)

##---- with spatial correlation structure ----
soil.gau <- update(soil.model, correlation = corGaus(1, form = ~ east + north), method = "ML")
summary(soil.gau)

# In this example, incorporating the Gaussian correlation structure 
# both improved the model fit and changed the nature of the regression model.
# Without the spatial structure, soil is a statistically significant predictor of thick.
# With the spatial structure, this relationship becomes not significant.
# This suggests that after controlling for location and the known correlation structure,
# soil does not add much new information.