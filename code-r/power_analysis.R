#####################
# power analysis
#####################

# libraries
library(pwr)
library(pwr2) # includes ss.2way = Sample size calculation for balanced two-way ANOVA models
library(ggplot2)

##---- set working directory ----
rm(list=ls())

normalizePath(readClipboard(),winslash = "/")
setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_20190620/code-r")

##---- 2-group example ----
mu.1 = 4692
stdev.1 = 300
mu.2 = 5137
stdev.2 = 400

# pooled standard deviation
stdev.pooled = (stdev.1+stdev.2)/2

# Effect size (Cohen's d)
# difference between the means divided by the pooled standard deviation
d = (mu.2-mu.1)/stdev.pooled

# calculation test power
pwr.t.test(d = d, sig.level = 0.01, power = 0.9)

##---- make plot ----
dvals <- seq(1, 10, length.out=100)
powvals <- sapply(dvals, function (x) ceiling(pwr.t.test(d = x, sig.level = 0.01, power = 0.9)$n))
data <- data.frame(dvals,powvals)

n.ggplot <- ggplot(data = data, aes(x = dvals, y = powvals)) +
  geom_line(colour="cornflowerblue", size = 2) +
  scale_x_continuous(limits=c(0.5,10),breaks=seq(1,10,1))+
  scale_y_continuous(limits=c(0,32),breaks=c(0,3,6,9,12,15,20,25,30,35))+
  xlab(label= c(expression(paste("Effect size Cohen's d"))))+
  ylab(label= c(expression(paste("Required sample size n"))))+
  annotate("text", x = 10, y = 31, label = as.character(c(expression(paste(alpha," = 0.01")))), parse=TRUE, size = 6, hjust=1, fontface = "plain", colour="black")+
  annotate("text", x = 10, y = 28, label = as.character(c(expression(paste("power = 0.90")))), parse=TRUE, size = 6, hjust=1, fontface = "plain", colour="black")+
  theme_bw() + # remove grey background
  theme(legend.position="none")+
  theme(legend.text = element_text(size = 10, colour = "black")) +
  theme(legend.title = element_text(size = 10, colour = "black")) +
  theme(axis.title.x = element_text(face="bold", size=14), axis.text.x  = element_text(size=12,angle = 0, vjust = 0.5)) +
  theme(axis.title.y = element_text(face="bold", size=14), axis.text.y  = element_text(size=12))

n.ggplot

##---- 2-factor anova example ----
ss.2way(a=2, b=2, alpha=0.05, beta=0.1, f.A=9, f.B=1,
        delta.A=NULL, delta.B=NULL, sigma.A=NULL, sigma.B=NULL, B=100)


