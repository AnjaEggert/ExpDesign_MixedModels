#####################
# 1-factor rcbd
# plants
#####################

library(xlsx)
library(ggplot2)
library(ggfortify)
library(emmeans) # also needs package multcompView to be installed

##---- set working directory ----
rm(list=ls())

normalizePath(readClipboard(),winslash = "/")
setwd("")

##---- read in data from excel file ----
dat <- read.xlsx("../data/1f_rcbd_plants.xlsx", 1)

# plot for first impression
plot()

# check data format, are block and nut = factors?
str(dat)

##---- Fit linear model ----
# fixed effects: nutrient + block
# Step 1: Check F-Test of ANOVA
# Step 2: Compare adjusted means per level

mod <- lm()

summary(mod)  # More detailed results

anova(mod)    # ANOVA-table: Variety effect is significant

##---- extract estimates of means per factor level ----
# get means and comparisons
means  <- emmeans(mod, pairwise ~ nut, adjust = "tukey")
means # look at means and differences between means

# add letters indicating significant differences between means
output <- CLD(means$emmeans, Letters=letters)


##---- plot adjusted means ----
output$.group <- gsub(" ", "", output$.group, fixed = TRUE) # remove spaces

q <- ggplot() +
  theme_classic()+
  geom_boxplot(data=dat, aes(x=diet, y=mass), outlier.shape=NA, width=0.6) + # Rohdaten (dat)
  geom_jitter(data=dat, aes(x=diet, y=mass), width=0.25, height=0, shape=1) + # Ergebnisse (output)
  geom_point(data=output, aes(x=as.numeric(diet)+0.4, y=emmean), col="red", shape=16, size=2) +
  geom_errorbar(data=output, aes(x=as.numeric(diet)+0.4, ymin=lower.CL, ymax=upper.CL), col="red", width=0.1) +
  geom_text(data=output, aes(x=as.numeric(diet)+0.5, y=emmean, label =.group), col="red")
q # show plot  
  
png("xx.png", width = 15, height = 10, units = 'cm', res = 300, type='cairo')
q
dev.off()
  
  
  