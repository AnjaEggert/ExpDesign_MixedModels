rm(list=ls())
setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_20190620/code-r")

library(data.table)
dt <- fread("../data/2f_splitplot_pig_food.txt") # directly import as data.table format via fread()

dt$Block <- as.factor(dt$Block)
dt$N     <- as.factor(dt$N)
dt$Var   <- as.factor(dt$Var)
dt$Yield <- as.numeric(dt$Yield)

# Split Plot Design 
# Two-Way ANOVA

### Split Plot Design 
# When some factors (independent variables) are difficult 
# or impossible to change in your experiment, a completely 
# randomized design isn't possible. The result is a 
# split-plot design, which has a mixture of hard to
# randomize (or hard-to-change) and easy-to-randomize 
# (or easy-to-change) factors. The hard-to-change factors 
# are implemented first, followed by the easier-to-change factors.

# boxplots for first impression
boxplot(data=dt, Yield ~ N + Var, las=2)
boxplot(data=dt, Yield ~ N      , las=2)
boxplot(data=dt, Yield ~ Var    , las=2)

# In a split-plot design, the (incomplete) mainplots should
# be taken as a "random effect". As a general principle, 
# each randomization units needs to be represented by a random effect, 
# so each randomization unit has its own error term. 
# Since we then have random and fixed effects
# in one model, we are fitting a "mixed model".
# In R the most common packages for that are "lme4", "nlme", "asreml-R" and "sommer".

# If you use lme4, always load the lmerTest package, too
#install.packages("lme4")
#install.packages("lmerTest")
library(lme4)
library(lmerTest)

# Fit general linear mixed model 
#################################
# Treatment effects: Variety, Fertilizer and their interaction
# Design effects:    Block and mainplot(=random effect)
# Step 1: Check F-Test of ANOVA and perform backwards elimination
# Step 2: Compare adjusted means per level
mod <- lmer(data    = dt,
            formula = Yield ~ N + Var + N:Var + 
                      Block + (1|Block:N))

# Note: In this example, Block*N identifies the incomplete blocks (=main plots) within each complete block. 
# To read more about this example, see p. 59 of Prof. Piepho's lecture notes for "Mixed models for metric data"

anova(mod) # Interaction effect significant - final model
# plot(mod)                              # residual plot 1
# qqnorm(resid(mod)); qqline(resid(mod)) # residual plot 2
mod          # Basic results
summary(mod) # More detailed results

# get adj. means for Variety effect and compare
library(emmeans)

# get means and comparisons
means <- emmeans(mod, pairwise ~ N | Var, adjust = "tukey") # to get t-test: adjust="none"
   # Note that N | Var gets pairwise N comparisons for each
   # Variety separately. You can use N:Var instead to get all
   # pairwise comparisons.

means # look at means and comparisons
means$emmeans   # look at means
means$contrasts # look at comparions

output <- CLD(means$emmeans, details=T, Letters = letters)
output # this data format is not good for ggplot
output <- as.data.table(output$emmeans) # reformatting into one table
output # this is better

# plot adjusted means
#install.packages("ggplot2")
library(ggplot2)

p <- ggplot(data=output, aes(x=N))
p <- p + geom_bar(aes(y=emmean), stat="identity", width=0.8) 
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.4)
p <- p + geom_text(aes(y=emmean+1500, label=.group))
p <- p + facet_wrap(~Var) # one per variety

p # show plot

# save ggplot as file into your working directory
ggsave("test.jpeg", width = 20, height = 10, units = "cm")
