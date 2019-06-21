rm(list=ls())

setwd("D:/Eggert/Documents/FBN/statistics_teaching/graduiertenakademie_hro_20190620/code-r")
dat <- read.delim("../data/2f_splitplot_pigs.txt")


str(dat)
# 'data.frame':	72 obs. of  4 variables:
# $ block: int  1 1 1 1 1 1 1 1 1 1 ...
# $ food : int  1 1 1 1 2 2 2 2 3 3 ...
# $ pig  : int  1 2 3 4 1 2 3 4 1 2 ...
# $ meat : int  4520 4034 3554 4216 5598 6682 4948 5372 5806 5738 ...

# Split-plot design
# 2 fixed factors = pig (4 leves) and food (6 levels)
# sorted as blocks
# measured trait = meat quality

library(agricolae)
library(desplot)

# These designs have two factors, one is applied in plots and is defined as trt1
# in a randomized complete block design;
# and a second factor as trt2, which is applied in the subplots of each plot applied at random.

pig<-c("pig1","pig2","pig3","pig4")
food<-c("food1","food2","food3","food4","food5","food6")

design <-design.split(food,pig,r=3,
                      design = "rcbd", serie=2,seed=543,
                      kinds = "Super-Duper",
                      first = TRUE, randomization = TRUE)

book <- design$book

str(book)
book$plots<-as.integer(book$plots)
book$splots<-as.integer(book$splots)


# export design as excel table
write.table(book[1:8,], "C:/Users/eggert/ownCloud/fbn_PhdCourse_2018_ExpDesign/analyses/FIG/2f_splitplot_pigs_design.txt",
            sep="\t", row.names = F) 

d1 <- desplot(data=book, form=block ~ plots+splots,
             out1=block,out2=plots,
             text=pig, cex=1, col=food,
             main="Split-Plot Design",show.key=T)
d1

png("C:/Users/eggert/ownCloud/fbn_PhdCourse_2018_ExpDesign/analyses/FIG/2f_splitplot_pigs_design.png", width = 15, height = 10, units = 'cm', res = 300, type='cairo')
d1
dev.off()
