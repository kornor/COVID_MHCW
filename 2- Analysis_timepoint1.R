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
tp1 <- read.csv("Data_tp1_cleaned.csv", header = TRUE, stringsAsFactors = TRUE, na.strings="")


## order the levels
levels(tp1$A_dass_score)
tp1$A_dass_score <- factor(tp1$A_dass_score, levels = c("Normal", "Mild", "Moderate", "Severe", "Extremely severe"))
tp1$D_dass_score <- factor(tp1$D_dass_score, levels = c("Normal", "Mild", "Moderate", "Severe", "Extremely severe"))
tp1$S_dass_score <- factor(tp1$S_dass_score, levels = c("Normal", "Mild", "Moderate", "Severe", "Extremely severe"))

levels(tp1$P_cbi_score)
tp1$P_cbi_score <- factor(tp1$P_cbi_score, levels = c("Low","Moderate", "High", "Severe"))
tp1$W_cbi_score <- factor(tp1$W_cbi_score, levels = c("Low","Moderate", "High", "Severe"))
tp1$C_cbi_score <- factor(tp1$C_cbi_score, levels = c("Low","Moderate", "High", "Severe"))

levels(tp1$Economic_impact)
tp1$Economic_impact <- factor(tp1$Economic_impact, levels = c("Very negative impact", "Negative impact", "Little or no impact", "Positive Impact", "Very positive impact"))
tp1$Social_impact <- factor(tp1$Social_impact, levels = c("Very negative impact", "Negative impact", "Little or no impact", "Positive Impact", "Very positive impact"))
tp1$Health_impact <- factor(tp1$Health_impact, levels = c("Very negative impact", "Negative impact", "Little or no impact", "Positive Impact", "Very positive impact"))
tp1$Workplace_impact <- factor(tp1$Workplace_impact, levels = c("Very negative impact", "Negative impact", "Little or no impact", "Positive Impact", "Very positive impact"))
tp1$Flexibility_impact <- factor(tp1$Flexibility_impact, levels = c("Very negative impact", "Negative impact", "Little or no impact", "Positive Impact", "Very positive impact"))


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



# DASS 
plot <- ggplot(tp1, aes(x=D_dass_val)) + geom_histogram(fill="grey")
plot

plot <- ggplot(tp1, )

##?? stacked bar plots with percetange of scores for each three domains?


plot <- ggplot(tp1, aes(x=w_cbi_score)) + geom_bar(fill="grey")
plot

plot <- ggplot(tp1, aes(x=P_cbi_score)) + geom_bar(fill="grey")
plot


ggplot(tp1, aes(x = Vulnerable, y = Anxiety_jan)) + geom_boxplot()



ggplot(tp1, aes(x  = A_dass_score, y = Anxiety_mar)) + geom_boxplot()
ggplot(tp1, aes(x  = A_dass_score, y = Anxiety_jan)) + geom_boxplot()


ggplot(tp1, aes(x  = P_cbi_score, y = Anxiety_mar)) + geom_boxplot()
ggplot(tp1, aes(x  = P_cbi_score, y = Anxiety_jan)) + geom_boxplot()
ggplot(tp1, aes(x  = W_cbi_score, y = Anxiety_mar)) + geom_boxplot()

### discpline
ggplot(tp1, aes(x  = Discipline, y = P_cbi_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Discipline, y = W_cbi_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Discipline, y = C_cbi_val)) + geom_boxplot()

ggplot(tp1, aes(x  = Discipline, y = D_dass_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Discipline, y = A_dass_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Discipline, y = S_dass_val)) + geom_boxplot()


## vulnerability
ggplot(tp1, aes(x  = Vulnerable, y = P_cbi_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Vulnerable, y = W_cbi_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Vulnerable, y = C_cbi_val)) + geom_boxplot()

ggplot(tp1, aes(x  = Vulnerable, y = D_dass_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Vulnerable, y = A_dass_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Vulnerable, y = S_dass_val)) + geom_boxplot()


## impact
ggplot(tp1, aes(x  = Workplace_impact, y = P_cbi_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Workplace_impact, y = W_cbi_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Workplace_impact, y = C_cbi_val)) + geom_boxplot()

ggplot(tp1, aes(x  = Workplace_impact, y = D_dass_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Workplace_impact, y = A_dass_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Workplace_impact, y = S_dass_val)) + geom_boxplot()


ggplot(tp1, aes(x  = Social_impact, y = P_cbi_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Social_impact, y = A_dass_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Health_impact, y = A_dass_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Economic_impact, y = D_dass_val)) + geom_boxplot()
ggplot(tp1, aes(x  = Economic_impact, y = A_dass_val)) + geom_boxplot()


## days off
ggplot(tp1, aes(x  = Days_absent, y = A_dass_val)) + geom_point()
ggplot(tp1, aes(x  = Days_absent, y = W_cbi_val)) + geom_point()

ggplot(tp1, aes(x  = Number_tests, y = A_dass_val)) + geom_point()
