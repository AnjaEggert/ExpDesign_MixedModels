#####################
# repeated measurements
#####################

##---- libraries ----
library(data.table)
library(emmeans)
library(ggplot2)
library(nlme)

##---- set working directory ----
rm(list=ls())

normalizePath(readClipboard(),winslash = "/")
setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_20190620/code-r")

##---- read in data from excel file ----
dat <- fread("../data/1f_rcbd_repmeas_fish.txt")

# format multiple columns as.factor in one step
makefactor <- names(dat)[1:3] # names of column that should be factor
dat[, (makefactor) := lapply(.SD, as.factor), .SDcols=makefactor] # define all those columns as.factor

# plots for first impression
plot(y=dat$growth, x=dat$gen)        # boxplot in plot()
ggplot(data=dat, aes(y=growth, x=gen)) + # same plot in ggplot()
  geom_boxplot()

ggplot(data=dat, aes(y=growth, x=gen, fill=week)) + # per week
  geom_boxplot()

### In this experiment, the observational units (=basin) were actually
### measured multiple times: once per week, over five weeks.
### This is called "repeated measures". 
### The aim of the experiment is to compare varieties.
### You have to include that due to the repeated measures, the error
### term of the model should be allowed to be correlated between weeks.


# Option 1: Analyze each week separately
########################################
# and thus avoid the repeated measures "problem"

# Analyzing only week 1:
mod.wk1 <- lm(data    = dat[week=="1"],
              formula = growth ~ gen + block)
anova(mod.wk1) # and so on ...

# Analyzing all 5 weeks separately in a loop
result.list <- list() # create an empty list object

for (week.nr in 1:5){ # start loop through week.nr = 1 to 5
  mod <- lm(data    = dat[week==week.nr], 
            formula = growth ~ gen + block)
  result.list[[week.nr]] <- anova(mod) # save anove into list
} # end loop

result.list      # show complete list of all 5 ANOVAs
result.list[[1]] # show first week ANOVA
result.list[[5]] # show fifth week ANOVA

# Option 2: Analyze across weeks 
################################
# and account for repeated measures "problem" via assuming
# a variance-covariance structure for the error term

# create a column that identifies a single plot
dat$plotID <- as.factor(paste0("v", dat$gen, "-r", dat$block))

### We fit 4 popular variance-covariance structures: ID, AR1, CS and UN

# ID: independent, uncorrelated random plot error
mod.id <- gls(data  = dat, 
              model = growth ~ week + week*gen + week*block)

# AR1: first-order autoregressive 
mod.ar <- gls(data  = dat, 
              model = growth ~ week + week*gen + week*block,
              corr  = corExp(form = ~ week|plotID))

# CS: compound symmetry
mod.cs <- gls(data  = dat, 
              model = growth ~ week + week*gen + week*block,
              corr  = corCompSymm(form = ~ week|plotID))

# UN: Unstructured
mod.un <- gls(data  = dat, 
              model = growth ~ week + week*gen + week*block,
              corr  = corSymm(form = ~ 1|plotID),
              weights = varIdent(form = ~ 1|week))

# Which model do I choose? 
# If models have the same fixed effects, but differ in their random/error part, 
# the Akaike Information Criterion (AIC) is an often used model selection 
# criterion. The smaller AIC the better is the model.

AIC(mod.id) # get the AIC for mod.id

AICs <- data.table(model = c("ID", "AR", "CS", "UN"),
                   AIC   = c(AIC(mod.id), AIC(mod.ar), AIC(mod.cs), AIC(mod.un)))
AICs[order(AIC)] # both AR and CS models are best.

# Choose AR model
#################

# residual plots
plot(mod.ar)                                
qqnorm(resid(mod.ar)); qqline(resid(mod.ar))

anova(mod.ar) # all effects significant - final model

# get means and comparisons
means <- emmeans(mod.ar, pairwise ~ week | gen, adjust = "tukey") # to get t-test: adjust="none"
# Note that week | var gets pairwise vat comparisons for each
# week separately. You can use weeK.var instead to get all
# pairwise comparisons.

means # look at means and comparisons
means$emmeans   # look at means
means$contrasts # look at comparions

output <- CLD(means$emmeans, details=T, Letters = letters)
output # this data format is not good for ggplot
output <- as.data.table(output$emmeans) # reformatting into one table
output # this is better

# plot adjusted means per week
p <- ggplot(data=output, aes(x=week))                        # dataset:output, x-axis: week
p <- p + geom_bar(aes(y=emmean), stat="identity")            # vertical bars of heigt in "emmean" column
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE))  # error bars from ymin to ymax
p <- p + geom_text(aes(y=emmean+0.5, label=.group))          # letters of ".group"-column
p <- p + facet_wrap(~gen)                                    # one plot per "gen"
p

# or

p <- ggplot(data=output, aes(x=gen, fill=week))
p <- p + geom_bar(aes(y=emmean), stat="identity",           position=position_dodge(width=1)) 
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), position=position_dodge(width=1))
p <- p + geom_text(aes(y=emmean+0.5, label=.group),         position=position_dodge(width=1.22))
p



