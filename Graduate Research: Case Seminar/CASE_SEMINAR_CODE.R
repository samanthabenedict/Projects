setwd()

#########  HUNTER COLLEGE: DEPARTMENT OF MATHEMATICS & STATISTICS 
#########  CASE SEMINAR
#########  FALL 2019
#########  Exploration of Systematic Biases in Phase III Clinical Trials
#########  Samantha Benedict


library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(zoo) 
library(chron) 
library(plyr)
library(xts)
library(lattice)
library(rugarch)
library(readxl)
library(tidyverse)
library(readxl)
library(writexl)
library(ggrepel)
library(data.table)
library(lmtest)
library(alr3)
library(GGally)
library(corrplot)
library(RColorBrewer)
library(leaps)
library(GGally)
library(corpcor)
library(mctest)
library(ppcor)
library(LearnBayes)
library(lattice)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(plyr)
library(readxl)
library(tidyverse)
#library(read.csv)
library(ggrepel)
library(data.table)
library(baytrends)
library(lmtest)
library(qualityTools)
library(alr3)
library(gmodels)
library(stats)
library(lmtest)


######## EXPERIMENTAL


#########  1. simualated input values 

# Scale for somenone having ashtham: The lower the asthma value, the more difficulty that person has breathing 
n = 860
mu = 100
sd = 10
mu.i = 200
weight.mu = 195
weight.sd = 15 



# fix the seed for random number generation 
set.seed(2019)

# use 'rnorm' to generate random normal 
weight = rnorm(n, weight.mu, weight.sd)
bp.base = rnorm(n, mu, sd)
bp.end = rnorm(n, mu, sd)

# take the difference between endpoint and balance 
bp.diff = bp.end-bp.base
# put the data together using "cbind" to column-bind
dat4placebo = round(cbind(weight, bp.base, bp.end, bp.diff))
head(dat4placebo)

# new asthma sample drug treatment
weight = rnorm(n, weight.mu, weight.sd)
bp.base = rnorm(n, mu, sd)
bp.end = rnorm(n, mu-mu.i, sd)

# take the difference between endpoint and balance 
bp.diff = bp.end-bp.base
# put the data together using "cbind" to column-bind
dat4drug = round(cbind(weight, bp.base, bp.end, bp.diff))
head(dat4drug)

#make a dataframe to hold all data
dat = data.frame(rbind(dat4placebo, dat4drug))
head(dat)

# make "trt" as a factor for treatement 
dat$trt = as.factor(rep(c("Placebo", "Drug"), each = n))
head(dat)

# check the data dimension
dim(dat)

# print the first 6 observations to see the variable names 
head(dat)

#########  2. simualated input values 
par(mar=c(1,1,1,1))
par(mfrow=c(2,1))
mynames<-c("Weight", "Start PF ", "End PF", "PF Diff.")
cols = brewer.pal(5, "Set1")
pal = colorRampPalette(cols)
boxplot(dat4placebo,las= 1,  main= "Placebo", col = pal(20),names=mynames, pch=20)

cols = brewer.pal(3, "Greens")
pal = colorRampPalette(cols)
boxplot(dat4drug,las= 1, col = pal(4),names=mynames,  pch=20, xaxt="n")
title("Drug", adj = 0.5, line = -14)

######### 3.  regression 
library(lattice)
library(xtable)

lm1 = lm(bp.diff ~ trt*weight, data = dat);summary(lm1)

print(xtable(lm1, caption= "ANOVA table simulated clinical trial data", label = "tab4RIcoeff"), table.placement = "htbp", caption.placement = "top")

# call xyplot functionand print it
print(xyplot(bp.diff~weight|trt, data = dat, col = "red", xlab= "Weight", strip = strip.custom(bg = "white"), ylab = "PF Difference",
             lwd = 3, cex= .5, pch= 20, type=c("p", "r")), las=1)




##########  4. plots 
layout(matrix(1:4, nrow=2 ))
#dev.new(width=5, height=4, unit="in")
dev.new(width = 3000, height = 600, unit = "px")
plot(lm1, pch=20, col= "darkblue", lwd= 3)

par(mfrow=c(1.1,1.1))
dev.new(width=5, height=4, unit="in")
dev.new(width = 550, height = 330, unit = "px")
plot(lm1, pch=20, col= "darkblue", lwd= 3, which=(5))



