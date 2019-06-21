#####################
# dragons example
#####################

##---- libraries ----
library(lme4)
library(ggplot2)

##---- set working directory ----
rm(list=ls())

normalizePath(readClipboard(),winslash = "/")
setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_20190620/code-r")

###---- Explore the data -----###

## load the data and have a look at it
load("../data/dragons.RData")

head(dragons)

## Does the body length affect test scores?

##---- distribution of data ---- 
hist(dragons$testScore)  # seems close to normal distribution - good!

# standardise your explanatory variables before proceeding
# scale()
dragons$bodyLength2 <- scale(dragons$bodyLength)

##---- Fit all data in one analysis -----

##---- fitting linear model to all ata, ignoring the sites + mountain ranges
basic.lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(basic.lm)

##---- plot the data with ggplot2 ----
a <-ggplot(dragons, aes(x = bodyLength, y = testScore)) +
  geom_point()+
  geom_smooth(method = "lm")

# check assumptions
plot(basic.lm, which = 1)  # not perfect, but look alright
plot(basic.lm, which = 2)  # a bit off at the extremes, but that's often the case; again doesn't look too bad


# Are the data independent?
# Multiple samples from eight mountain ranges
# ata from within each mountain range can be more similar to each other than the data from different mountain ranges - they are correlated.

# plot the data
boxplot(testScore ~ mountainRange, data = dragons)  # certainly looks like something is going on here

ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange))+
  geom_point(size = 2)+
  theme_classic()+
    theme(legend.position = "none")

# plots look like mountain ranges vary both in the dragon body length and in their test scores.
# This confirms that our observations from within each of the ranges aren't independent. We can't ignore that.

##----- Run multiple analyses ----
# run many separate analyses and fit a regression for each of the mountain ranges.
# facet_wrap()

ggplot(aes(bodyLength, testScore), data = dragons) + geom_point() +
    facet_wrap(~ mountainRange) +
    xlab("length") + ylab("test score")

##----- Modify the model, add mountains as fixed factor -----

## We want to use all the data, but account for the data coming from different mountain ranges
mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)

## now body length is not significant


###----- Mixed effects models -----###
mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)

plot(mixed.lmer)  # looks alright, no paterns evident
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!

# Once we account for the mountain ranges, it's obvious that dragon body length doesn't actually explain the differences in the test scores.

##---- variance accounted for by mountain ranges ----
339.7/(339.7 + 223.8)  # ~60 %

### create new "sample" variable
dragons <- within(dragons, sample <- factor(mountainRange:site))

##----- Second mixed model -----##
mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)

mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
  facet_wrap(~mountainRange, nrow=3) +
  geom_point() +
  theme_classic() +
  geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred)) +
  theme(legend.position = "none")

mm_plot


##----- Model selection for the keen -----##

### full model

### reduced model

### comparison
