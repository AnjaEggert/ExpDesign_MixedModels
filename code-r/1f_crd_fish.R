#####################
# 1-factor crd
# fish
#####################

##---- libraries ----
library(desplot)
library(agricolae)
library(ggplot2)
library(ggfortify)
library(emmeans) # also needs package multcompView to be installed

##---- set working directory ----
rm(list=ls())

normalizePath(readClipboard(),winslash = "/")
setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_20190620/code-r")

##---- make experimental design ----
diet <-c("A","B","C","D")
# repeats <-c(4,4,4,4) # only necessary if not same number in each treatment
# seed = 12543
design <-design.crd(diet,4,serie=1,seed=42,"Super-Duper")
dat<-design$book

dat$col<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
dat$row<-rep(seq(1,4),4)

##---- type in data manually ----
dat$mass <- c(62,60,61,63,66,67,63,56,62,54,62,61,58,59,64,61)

str(dat)
# 'data.frame':	16 obs. of  6 variables:
# $ plots: num  11 12 13 14 15 16 17 18 19 20 ...
# $ r    : int  1 2 1 1 2 3 3 2 1 3 ...
# $ diet : Factor w/ 4 levels "A","B","C","D": 4 4 1 2 2 4 2 1 3 1 ...
# $ col  : num  1 1 1 1 2 2 2 2 3 3 ...
# $ row  : int  1 2 3 4 1 2 3 4 1 2 ...
# $ mass : num  62 60 61 63 66 67 63 56 62 54 ...

# plot for first impression
plot(y=dat$mass, x=dat$diet)

##---- plot experimental design ----
mycolors <- c("cornflowerblue","goldenrod1","firebrick1", "darkolivegreen3")

design.plot<- desplot(data=dat, form=diet ~ col+row,
                      text = diet, cex= 1,col.regions=mycolors,out1=plots,
                      main="Completely Randomized Design",show.key=F)  
design.plot

png("../FIG/1f_crd_fish_design.png", width = 10, height = 10, units = 'cm', res = 300, type='cairo')
design.plot
dev.off()

##---- Fit linear model ----
# Fixed effect: diet
# Step 1: Check F-Test of ANOVA
# Step 2: Compare adjusted means per level
mod <- lm(formula = mass ~ diet, data = dat)

autoplot(mod) # Residual plots
mod           # Basic results
#Call:
# lm(formula = mass ~ diet, data = dat)
# 
# Coefficients:
# (Intercept)        dietB        dietC        dietD  
#       57.25         5.50         4.25         6.00  

summary(mod)  # More detailed results
# Call:
#   lm(formula = mass ~ diet, data = dat)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -3.75  -1.25   0.25   0.75   3.75 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   57.250      1.285  44.555 1.06e-14 ***
#   dietB          5.500      1.817   3.027  0.01053 *  
#   dietC          4.250      1.817   2.339  0.03747 *  
#   dietD          6.000      1.817   3.302  0.00632 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.57 on 12 degrees of freedom
# Multiple R-squared:  0.5295,	Adjusted R-squared:  0.4119 
# F-statistic: 4.502 on 3 and 12 DF,  p-value: 0.02455

anova(mod)    # ANOVA-table: Variety effect is significant
# Analysis of Variance Table
# 
# Response: mass
#           Df Sum Sq Mean Sq F value  Pr(>F)  
# diet       3 89.188 29.7292  4.5016 0.02455 *
# Residuals 12 79.250  6.6042                  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##---- extract estimates of means per factor level ----
# get means and comparisons
means  <- emmeans(mod, pairwise ~ diet, adjust = "tukey") # to get t-test: adjust="none"
means # look at means and differences between means

# $`emmeans`
# diet emmean       SE df lower.CL upper.CL
# A     57.25 1.284929 12 54.45038 60.04962
# B     62.75 1.284929 12 59.95038 65.54962
# C     61.50 1.284929 12 58.70038 64.29962
# D     63.25 1.284929 12 60.45038 66.04962
# 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast estimate       SE df t.ratio p.value
# A - B       -5.50 1.817164 12  -3.027  0.0452
# A - C       -4.25 1.817164 12  -2.339  0.1434
# A - D       -6.00 1.817164 12  -3.302  0.0280
# B - C        1.25 1.817164 12   0.688  0.8997
# B - D       -0.50 1.817164 12  -0.275  0.9923
# C - D       -1.75 1.817164 12  -0.963  0.7723
# 
# P value adjustment: tukey method for comparing a family of 4 estimates 

# add letters indicating significant differences between means
output <- CLD(means$emmeans, Letters=letters)

##---- plot adjusted means - option 1 ----
p <- ggplot(data=output, aes(x=diet))+
  geom_bar(aes(y=emmean), stat="identity", width=0.8)+
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.4)+
  geom_text(aes(y=emmean+5, label=.group))
p # show plot

##---- plot adjusted means - option 2 ----
output$.group <- gsub(" ", "", output$.group, fixed = TRUE) # remove spaces

q <- ggplot() +
  theme_classic()+
  geom_boxplot(data=dat, aes(x=diet, y=mass), outlier.shape=NA, width=0.6) + # Rohdaten (dat)
  geom_jitter(data=dat, aes(x=diet, y=mass), width=0.25, height=0, shape=1) + # Ergebnisse (output)
  geom_point(data=output, aes(x=as.numeric(diet)+0.4, y=emmean), col="red", shape=16, size=2) +
  geom_errorbar(data=output, aes(x=as.numeric(diet)+0.4, ymin=lower.CL, ymax=upper.CL), col="red", width=0.1) +
  geom_text(data=output, aes(x=as.numeric(diet)+0.5, y=emmean, label =.group), col="red")
q # show plot  
  
png("../FIG/1f_crd_fish_results.png", width = 15, height = 10, units = 'cm', res = 300, type='cairo')
q
dev.off()
  
  
  