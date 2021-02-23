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
library(ggsignif)
library (ggpubr)
library(RColorBrewer)
library(dplyr)
library(Hmisc)
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

tp1$Vulnerable <- factor(tp1$Vulnerable, levels = c("Yes","No", "Unsure"))
## descriptive
## Age distribution
summary(tp1$Age)
plot(tp1$Age)

## Gender
summary(tp1$Gender)

## Occupational discpline
summary(tp1$Discipline)
summary(tp1$Medical_role)

# testing
summary(tp1$Vulnerable)
summary(tp1$Days_absent)
summary(tp1$Number_tests)
mode(tp1$Number_tests)
describe(tp1$Days_absent)
describe(tp1$Number_tests)

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

ggplot(tp1, aes(Anxiety_jan)) + geom_bar()


# pivot / gather
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long <- gather(tp1, Timepoint, Anxiety, Anxiety_mar:Anxiety_jan, factor_key=TRUE)

# boxplot 
levels()
p <- ggplot(data_long, aes(x=Timepoint, y=Anxiety, fill = Vulnerable)) + 
  geom_boxplot() +
  geom_signif(comparisons = list(c("Anxiety_mar", "Anxiety_jan")), map_signif_level=TRUE) 
  #geom_signif(stat = identity, map_signif_level=TRUE)
p

plot <- ggplot(data_long, aes(Timepoint, Anxiety)) +
  geom_bar(aes(fill = Vulnerable), stat="identity", position="dodge", width=.5) +
  geom_signif(stat="identity",
              data=data.frame(x=c(0.875, 1.875), xend=c(1.125, 2.125),
                              y=c(105, 95), annotation=c("**", "NS")),
              aes(x=x,xend=xend, y=y, yend=y, annotation=annotation)) +
  geom_signif(comparisons=list(c("S1", "S2")), annotations="***",
              y_position = 125, tip_length = 0, vjust=0.4) +
  scale_fill_manual(values = c("grey80", "grey20"))
plot

# "p.signif" is stars
# p.format = the value
# no comment, the test and the value

p <- ggplot(data_long, aes(x=Timepoint, y=Anxiety, fill = Vulnerable)) + 
  geom_boxplot() +
  stat_compare_means(comparisons = list(c("Anxiety_mar", "Anxiety_jan")),label.y = 120) +
  #stat_compare_means() +
  stat_compare_means(aes(group = Vulnerable),label = "p.signif", label.y = c(105, 100))
  
  p


## K-w analysis 

## vulnerability leads to persistence of anxiety?

## anxiety vs testing time off?


## main concerns
summary(tp1$Risk.of.becoming.infected.in.the.workplace)
summary(tp1$Risk.of.passing.infection.onto.others..such.as.family.)
summary(tp1$Concern.about.PPE.or.working.conditions)
summary(tp1$Increased.workload)
summary(tp1$Increased.acuity...distress.of.clients)
summary(tp1$Fear.of.redeployment.to.other.services)


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



ggplot(tp1, aes(x  = A_dass_score, y = Anxiety_mar)) + geom_boxplot() + stat_compare_means()
ggplot(tp1, aes(x  = A_dass_score, y = Anxiety_jan)) + geom_boxplot() + stat_compare_means()


ggplot(tp1, aes(x  = P_cbi_score, y = Anxiety_mar)) + geom_boxplot()
ggplot(tp1, aes(x  = P_cbi_score, y = Anxiety_jan)) + geom_boxplot()
ggplot(tp1, aes(x  = W_cbi_score, y = Anxiety_mar)) + geom_boxplot()

### discpline
ggplot(tp1, aes(x  = Discipline, y = P_cbi_val)) + geom_boxplot() + stat_compare_means()
ggplot(tp1, aes(x  = Discipline, y = W_cbi_val)) + geom_boxplot() + stat_compare_means()
ggplot(tp1, aes(x  = Discipline, y = C_cbi_val)) + geom_boxplot() + stat_compare_means()

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



### Stacked bar of DASS severeity percentages

# long
df_long <- gather(tp1, Dimension, Score, c(29,31,33), factor_key = TRUE)
### set the levels again for "Score"
df_long$Score <- factor(df_long$Score, levels = c("Extremely severe", "Severe", "Moderate", "Mild", "Normal"))



##
ggplot(df_long %>% count(Dimension, Score) %>%    # Group by dimension and score, then count number in each group
         mutate(pct=n/sum(n),               # Calculate percent within each region
                ypos = cumsum(n) - 0.5*n),  # Calculate label positions
       aes(Dimension, n, fill=Score)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.5))


#### and for the CBI
df_long <- gather(tp1, Dimension, Score, c(35,37,39), factor_key = TRUE)


df_long$Score <- factor(df_long$Score, levels = c("Severe","High", "Moderate", "Low"))

ggplot(df_long %>% count(Dimension, Score) %>%    # Group by dimension and score, then count number in each group
                mutate(pct=n/sum(n),               # Calculate percent within each region
                ypos = cumsum(n) - 0.5*n),  # Calculate label positions
       aes(Dimension, n, fill=Score)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), ## positions the percent labels
            position=position_stack(vjust=0.5))
