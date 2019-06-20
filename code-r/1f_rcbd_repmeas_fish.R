#####################
# repeated measures
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

##---- read in data with read.table()----
# 1f_rcbd_repmeas_fish.txt"

# are all effects factors? If not, convert them


##---- plots for first impression ----
ggplot(data=dat, aes(y=growth, x=species)) + # same plot in ggplot()
  geom_boxplot()

# geom_boxplot per week


### In this experiment, the observational units (=basin) were actually
### measured multiple times: once per week, over five weeks. This is called
### "repeated measures" or "repeated measurements". 
### Due to the repeated measures, the error term of the model should be allowed to be correlated between weeks.


# Option 1: Analyze each week separately with lm()
# species + block as fixed effects

########################################
# and thus avoid the repeated measures "problem"

# Analyzing only week 1:
# subsetting dat:
dat[which(dat$week=="1"),]


# Analyzing all 5 weeks separately in a loop
result.list <- list() # create an empty list object

for (week.nr in 1:5){ # start loop through week.nr = 1 to 5
  mod <- lm(data    = dat[which(dat$week==week.nr),], 
            formula = growth ~ species + block)
  result.list[[week.nr]] <- anova(mod) # save anove into list
} # end loop

result.list      # show complete list of all 5 ANOVAs
result.list[[1]] # show first week ANOVA
result.list[[5]] # show fifth week ANOVA

# Option 2: Analyze across weeks 
################################
# and account for repeated measures "problem" via assuming
# a variance-covariance structure for the error term

# create a new column that identifies a single plot
dat$plotID <- as.factor(paste0("v", dat$species, "-r", dat$block))

### We fit 3 popular variance-covariance structures: ID, AR1, CS

# ID: independent, uncorrelated random plot error
mod.id <- gls(data  = dat, 
              model = growth ~ week + week*species + week*block)

# AR1: first-order autoregressive 
mod.ar <- gls(data  = dat, 
              model = growth ~ week + week*species + week*block,
              corr  = corExp(form = ~ week|plotID))

# CS: compound symmetry
mod.cs <- gls(data  = dat, 
              model = growth ~ week + week*species + week*block,
              corr  = corCompSymm(form = ~ week|plotID))

# Which model do I choose? 
# If models have the same fixed effects, but differ in their random/error part, 
# the Akaike Information Criterion (AIC) is an often used model selection 
# criterion. The smaller AIC the better is the model.

AIC() # get the AIC for mod.id


# Choose AR model
#################

# residual plots
plot(mod.ar)                                

anova(mod.ar) # all effects significant - final model

# get means and pairwise comparisons
# week | species gets pairwise species comparisons for each week separately


output <- CLD(means$emmeans, details=T, Letters = letters)
output # this data format is not good for ggplot
output <- as.data.table(output$emmeans) # reformatting into one table
output # this is better

# plot adjusted means per week
p <- ggplot(data=output, aes(x=week))                        # dataset:output, x-axis: week
p <- p + geom_bar(aes(y=emmean), stat="identity")            # vertical bars of heigt in "emmean" column
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE))  # error bars from ymin to ymax
p <- p + geom_text(aes(y=emmean+0.5, label=.group))          # letters of ".group"-column
p <- p + facet_wrap(~species)                                    # one plot per "species"
p

# or

p <- ggplot(data=output, aes(x=species, fill=week))
p <- p + geom_bar(aes(y=emmean), stat="identity",           position=position_dodge(width=1)) 
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), position=position_dodge(width=1))
p <- p + geom_text(aes(y=emmean+0.5, label=.group),         position=position_dodge(width=1.22))
p



