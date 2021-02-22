### COVID19_MHCW study
## initial timepoint, Jan-Feb 2020

### Data analysis and graphing file

#setwd
setwd("~/BioInformatics Work/Random Psych/COVID19_MHCW/COVID_MHCW")

## libraries
library(tidyverse)
library(plyr)
library(likert)
library(ggplot2)

## read in the data from timepoint 1
# make strings factors, and force empty cells into NAs
tp1 <- read.csv("Data_tp1_2.csv", header = TRUE, stringsAsFactors = TRUE, na.strings="")


## descriptive
## Age distribution
summary(tp1$Age)
plot(tp1$Age)

## Gender
summary(tp1$Gender)

## Occupational discpline
summary(tp1$Discipline)

# Medical Role

## Site


## Anxiety
# overall Mar
summary(tp1$Anxiety_mar)


# overall Jan

## difference statistically?



## by vulnerability
#plot
ggplot(tp1, aes(x = Vulnerable, y = Anxiety_mar)) + geom_boxplot()
ggplot(tp1, aes(x = Vulnerable, y = Anxiety_jan)) + geom_boxplot()

# pivot



## vulnerability leads to persistence of anxiety?

## anxiety vs testing time off?


## main concerns




#### likert 

## create factors with set ordered levels
tp1[21:26] <- lapply(tp1[21:26], 
                     factor, levels = c("Very negative impact","Negative impact", "Little or no impact","Positive impact","Very positive impact"),
                     ordered = TRUE)

## create a likert but no grouping
likt <- likert(tp1[,c(21:26)])
plot(likt)

# create a likert dataset from this, grouped by ...
likt_dis <- likert(tp1[,c(21:26)], grouping = tp1$Discipline)
# then plot
plot(likt_dis)

##different grouping
likt_vul <- likert(tp1[,c(21:26)], grouping = tp1$Vulnerable)
# then plot
plot(likt_vul)

##different grouping
likt_site <- likert(tp1[,c(21:26)], grouping = tp1$Site)
# then plot
plot(likt_site)
