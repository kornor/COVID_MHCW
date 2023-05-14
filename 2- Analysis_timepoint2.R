### COVID19_MHCW study
## initial timepoint, Jan-Feb 2020

### Data analysis and graphing file

#setwd
setwd("~/BioInformatics Work/Psychiatry Bioinformatics/COVID19_MHCW/COVID_MHCW/Timepoint 2")

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
library(pwr)
## read in the data from timepoint 2
# make strings factors, and force empty cells into NAs
tp1 <- read.csv("Data_tp1_cleaned_v2.csv", header = TRUE, stringsAsFactors = TRUE, na.strings="")
# make strings factors, and force empty cells into NAs
tp2 <- read.csv("Data_tp2_cleaned.csv", header = TRUE, stringsAsFactors = TRUE, na.strings="")


## order the levels
levels(tp2$A_dass_score)
tp2$A_dass_score <- factor(tp2$A_dass_score, levels = c("Normal", "Mild", "Moderate", "Severe", "Extremely severe"))
tp2$D_dass_score <- factor(tp2$D_dass_score, levels = c("Normal", "Mild", "Moderate", "Severe", "Extremely severe"))
tp2$S_dass_score <- factor(tp2$S_dass_score, levels = c("Normal", "Mild", "Moderate", "Severe", "Extremely severe"))

levels(tp2$P_cbi_score)
tp2$P_cbi_score <- factor(tp2$P_cbi_score, levels = c("Low","Moderate", "High", "Severe"))
tp2$W_cbi_score <- factor(tp2$W_cbi_score, levels = c("Low","Moderate", "High", "Severe"))
tp2$C_cbi_score <- factor(tp2$C_cbi_score, levels = c("Low","Moderate", "High", "Severe"))

levels(tp2$Economic_impact)
tp2$Economic_impact <- factor(tp2$Economic_impact, levels = c("Very negative impact", "Negative impact", "Little or no impact", "Positive Impact", "Very positive impact"))
tp2$Social_impact <- factor(tp2$Social_impact, levels = c("Very negative impact", "Negative impact", "Little or no impact", "Positive Impact", "Very positive impact"))
tp2$Health_impact <- factor(tp2$Health_impact, levels = c("Very negative impact", "Negative impact", "Little or no impact", "Positive Impact", "Very positive impact"))
tp2$Workplace_impact <- factor(tp2$Workplace_impact, levels = c("Very negative impact", "Negative impact", "Little or no impact", "Positive Impact", "Very positive impact"))
tp2$Flexibility_impact <- factor(tp2$Flexibility_impact, levels = c("Very negative impact", "Negative impact", "Little or no impact", "Positive Impact", "Very positive impact"))

tp2$Vulnerable <- factor(tp2$Vulnerable, levels = c("Yes","No", "Unsure"))
## descriptive
## Age distribution
summary(tp2$Age)
plot(tp2$Age)

shapiro.test(tp2$Age)

## Gender
summary(tp2$Gender)

## Occupational discpline
summary(tp2$Discipline)
summary(tp2$Medical_role)

# testing
summary(tp2$Vulnerable)
summary(tp2$Days_absent)
summary(tp2$Number_tests)
mode(tp2$Number_tests)
describe(tp2$Days_absent)
describe(tp2$Number_tests)
summary(tp2$Covid.Pos)
summary(tp2$Covid.Pos.Times)

## Site




## Anxiety by vulnerability
#compaire 
t.test(tp2$Anxiety.Jan.21, tp2$Anxiety.Now)

#plot
ggplot(tp2, aes(x = Vulnerable, y = Anxiety.Jan.21)) + geom_boxplot()
ggplot(tp2, aes(x = Vulnerable, y = Anxiety.Now)) + geom_boxplot()

ggplot(tp2, aes(Anxiety.Now)) + geom_bar()



# pivot / gather
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long <- gather(tp2, Timepoint, Anxiety, Anxiety.Now:Anxiety.Jan.21, factor_key=TRUE)

# boxplot 
levels()
p <- ggplot(data_long, aes(x=Timepoint, y=Anxiety, fill = Vulnerable)) + 
  geom_boxplot() +
  geom_signif(comparisons = list(c("Anxiety.Now", "Anxiety.Jan.21")), map_signif_level=TRUE) 
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
my_comparisons <- list( c("Yes", "No"), c("Yes", "Unsure"), c("No", "Unsure") )
my_comparisons <- list(c("Anxiety.Jan.21", "Anxiety.Now"))
levels(data_long$Timepoint)
data_long$Timepoint <- fct_rev(data_long$Timepoint)

p <- 
  ggplot(data_long, aes(x=Timepoint, y=Anxiety, fill = Vulnerable)) + 
  geom_boxplot() +
  stat_compare_means(comparisons =  my_comparisons, label = "p.format",label.y = 125, size = 6) +
  stat_compare_means(aes(group = Vulnerable),label = "p.format", label.y = c(105, 105), size = 6) +
  ggtitle("Self-Reported Anxiety \nby identification as vulnerable employee") +
  xlab("Timepoint") + ylab("Self-Reported Anxiety \n(between 0 - 100)") +
  labs(fill = "Respondant identifies \nas vulnerable employee") +
  scale_x_discrete(labels=c("Anxiety.Jan.21" = "January 2021 (retrospective)", "Anxiety.Now" = "January 2022 (contemporary)")) +
  theme(legend.position = "none", text = element_text(size=20))
                            
p


p <- ggplot(data_long, aes(x=Timepoint, y=Anxiety, fill = Vulnerable)) + 
  geom_boxplot() +
  stat_compare_means(label = "p.format",label.y = 120) +
  stat_compare_means(aes(group = Vulnerable),label = "p.format", label.y = c(105, 105)) +
  ggtitle("Self-Reported Anxiety \nby identification as vulnerable employee") +
  xlab("Timepoint") + ylab("Self-Reported Anxiety \n(between 0 - 100)") +
  labs(fill = "Respondant identifies \nas vulnerable employee") +
  scale_x_discrete(labels=c("Anxiety.Jan.21" = "January 2021 (retrospective)", "Anxiety.Now" = "January 2022 (contemporary)")) +
  theme(legend.position = "none")

p

#### let's try one timepoint at a time
  
my_comparisons <- list( c("Yes", "No"), c("Yes", "Unsure"), c("No", "Unsure") )
  p <- ggplot(tp2, aes(x=Vulnerable, y=Anxiety.Now, fill = Vulnerable)) + 
    geom_boxplot() +
    #stat_compare_means(comparisons = list(c("Anxiety_mar", "Anxiety_jan")),label.y = 120) +
    #stat_compare_means() +
    stat_compare_means(comparisons = my_comparisons, label = "p.signif")
  p

  
  
  my_comparisons <- list( c("Yes", "No"), c("Yes", "Unsure"), c("No", "Unsure") )
  p <- ggplot(tp2, aes(x=Vulnerable, y=Anxiety.Jan.21, fill = Vulnerable)) + 
    geom_boxplot() +
    #stat_compare_means(comparisons = list(c("Anxiety_mar", "Anxiety_jan")),label.y = 120) +
    #stat_compare_means() +
    stat_compare_means(comparisons = my_comparisons, label = "p.signif")
  p

## K-w analysis 

## vulnerability leads to persistence of anxiety?

  
  
######### Concerns leading to anxiety  

## main concerns
summary(tp2$Risk.of.becoming.infected.in.the.workplace)
summary(tp2$Risk.of.passing.infection.onto.others..such.as.family.)
summary(tp2$Concern.about.PPE.or.working.conditions)
summary(tp2$Increased.workload)
summary(tp2$Increased.acuity...distress.of.clients)
summary(tp2$Fear.of.redeployment.to.other.services)


#### likert 

## create factors with set ordered levels
responses <- lapply(tp2[22:27], 
            factor, levels = c("Very negative impact","Negative impact", "Little or no impact","Positive impact","Very positive impact"),
            ordered = TRUE)
responses <- as.data.frame(responses)

## create a likert but no grouping
likt <- likert(responses)
plot(likt)

plot(likt, text.size=8, 
     ordered=FALSE, 
     group.order=names(responses)) + 
      theme(axis.text=element_blank(),
        legend.text=element_text(size=16), 
        legend.title = element_blank(), 
        axis.title.x = element_text(size = 18))

likert(likt) %>% plot(group.order = names(.$items))

##
responses <- tp2[22:27]
mylevels <- c("Very negative impact","Negative impact", "Little or no impact","Positive impact","Very positive impact")

for(i in seq_along(responses)) {
  responses[,i] <- factor(responses[,i], levels=mylevels)
}

colnames(responses)<- c("Workplace Culture","Work Flexibility","Specialist Training *", "Economic Circumstances", "Social Life", "Health and Healthcare")

likt <- likert(responses)
plot(likt)

plot(likt, text.size=4, ordered=FALSE, group.order=names(responses)) + 
  labs(title = "Impact of the COVID19 pandemic on different domains of life", caption = "*Responses from psychiatric registrars only") +
  theme(plot.title = element_text(size = 18, hjust = -0.8),axis.text=element_text(size=12), legend.text=element_text(size=10)) 

ggsave( "plot.pdf", plot )

plot(likt, text.size=4, ordered=FALSE, group.order=names(responses)) + 
  theme(plot.title = element_text(size = 18, hjust = 8.0),axis.text=element_text(size=12), legend.text=element_text(size=10)) +
  ggtitle("Impact of the COVID19 pandemic on different domains of life")
  
  labs(title = "Impact of the COVID19 pandemic on different domains of life", caption = "*Responses from medical registrars only")



# create a likert dataset from this, grouped by ...
likt_dis <- likert(tp2[,c(21:26)], grouping = tp2$Discipline)
# then plot
plot(likt_dis)

##different grouping
likt_vul <- likert(tp2[,c(21:26)], grouping = tp2$Vulnerable)
# then plot
plot(likt_vul)

##different grouping
likt_site <- likert(tp2[,c(21:26)], grouping = tp2$Site)
# then plot
plot(likt_site)



# DASS 
plot <- ggplot(tp2, aes(x=D_dass_val)) + geom_histogram(fill="grey")
plot

plot <- ggplot(tp2, )

plot <- ggplot(tp2, aes(x=D_dass_score)) + geom_bar(fill="grey")
plot
plot <- ggplot(tp2, aes(x=A_dass_score)) + geom_bar(fill="grey")
plot

plot <- ggplot(tp2, aes(x=S_dass_score)) + geom_bar(fill="grey")
plot


##?? stacked bar plots with percetange of scores for each three domains?


plot <- ggplot(tp2, aes(x=W_cbi_score)) + geom_bar(fill="grey")
plot

plot <- ggplot(tp2, aes(x=P_cbi_score)) + geom_bar(fill="grey")
plot


ggplot(tp2, aes(x = Vulnerable, y = Anxiety_jan)) + geom_boxplot()



ggplot(tp2, aes(x  = A_dass_score, y = Anxiety.Now)) + geom_boxplot() + stat_compare_means()
ggplot(tp2, aes(x  = A_dass_val, y = Anxiety.Now)) + geom_point()

cor.test(as.numeric(tp2$Anxiety.Jan.21), as.numeric(tp2$A_dass_val))
## there is a correlation between anxiety at Jan 21 and A dass
cor.test(as.numeric(tp2$Anxiety.Now), as.numeric(tp2$A_dass_val))
## no correlation with covid anixety now and A DASS -> anxiety has become decoupled from covid anxiety


ggplot(tp2, aes(x  = A_dass_score, y = Anxiety.Jan.21)) + geom_boxplot() + stat_compare_means()


ggplot(tp2, aes(x  = P_cbi_score, y = Anxiety_mar)) + geom_boxplot()
ggplot(tp2, aes(x  = P_cbi_score, y = Anxiety_jan)) + geom_boxplot()
ggplot(tp2, aes(x  = W_cbi_score, y = Anxiety_mar)) + geom_boxplot()

### discpline
ggplot(tp2, aes(x  = Discipline, y = P_cbi_val)) + geom_boxplot() + stat_compare_means()
ggplot(tp2, aes(x  = Discipline, y = W_cbi_val)) + geom_boxplot() + stat_compare_means()
ggplot(tp2, aes(x  = Discipline, y = C_cbi_val)) + geom_boxplot() + stat_compare_means()

ggplot(tp2, aes(x  = Discipline, y = D_dass_val)) + geom_boxplot()
ggplot(tp2, aes(x  = Discipline, y = A_dass_val)) + geom_boxplot()
ggplot(tp2, aes(x  = Discipline, y = S_dass_val)) + geom_boxplot()


## vulnerability
ggplot(tp2, aes(x  = Vulnerable, y = P_cbi_val)) + geom_boxplot()
ggplot(tp2, aes(x  = Vulnerable, y = W_cbi_val)) + geom_boxplot()
ggplot(tp2, aes(x  = Vulnerable, y = C_cbi_val)) + geom_boxplot()

ggplot(tp2, aes(x  = Vulnerable, y = D_dass_val)) + geom_boxplot()
ggplot(tp2, aes(x  = Vulnerable, y = A_dass_val)) + geom_boxplot()
ggplot(tp2, aes(x  = Vulnerable, y = S_dass_val)) + geom_boxplot()


## impact
ggplot(tp2, aes(x  = Workplace_impact, y = P_cbi_val)) + geom_boxplot()
ggplot(tp2, aes(x  = Workplace_impact, y = W_cbi_val)) + geom_boxplot()
ggplot(tp2, aes(x  = Workplace_impact, y = C_cbi_val)) + geom_boxplot()

ggplot(tp2, aes(x  = Workplace_impact, y = D_dass_val)) + geom_boxplot()
ggplot(tp2, aes(x  = Workplace_impact, y = A_dass_val)) + geom_boxplot()
ggplot(tp2, aes(x  = Workplace_impact, y = S_dass_val)) + geom_boxplot()


ggplot(tp2, aes(x  = Social_impact, y = P_cbi_val)) + geom_boxplot()
ggplot(tp2, aes(x  = Social_impact, y = A_dass_val)) + geom_boxplot()
ggplot(tp2, aes(x  = Health_impact, y = A_dass_val)) + geom_boxplot()
ggplot(tp2, aes(x  = Economic_impact, y = D_dass_val)) + geom_boxplot()
ggplot(tp2, aes(x  = Economic_impact, y = A_dass_val)) + geom_boxplot()


## days off
ggplot(tp2, aes(x  = Days_absent, y = A_dass_val)) + geom_point()
ggplot(tp2, aes(x  = Days_absent, y = W_cbi_val)) + geom_point()

ggplot(tp2, aes(x  = Number_tests, y = A_dass_val)) + geom_point()



### Stacked bar of DASS severeity percentages

# long
df_long <- gather(tp2, Dimension, Score, c(48,50,52), factor_key = TRUE)

levels(df_long$Score)

### set the levels again for "Score"
df_long$Score <- factor(df_long$Score, levels = c("Extremely severe", "Severe", "Moderate", "Mild", "Normal"))

with(df_long, table(Dimension, Score)/134) 
df_long %>% dplyr::count(Dimension, Score)

##
ggplot(df_long %>% dplyr::count(Dimension, Score) %>%    # Group by dimension and score, then count number in each group
         mutate(pct=n/138,               # Calculate percent within each region
                ypos = cumsum(n) - 0.5*n),  # Calculate label positions
       aes(Dimension, n, fill=Score)) +
  theme(plot.title = element_text(size = 18),axis.text=element_text(size=12),axis.title=element_text(size=14), legend.text=element_text(size=10)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position=position_stack(vjust=0.5)) +
  ggtitle("DASS Domain Subscores") +
  xlab("Domain") + ylab("Percentage of cohort") +
  labs(fill = "Subscore", tag = "A") +
  scale_x_discrete(labels=c("D_dass_score" = "Depression", "A_dass_score" = "Anxiety", "S_dass_score" = "Stress")) 

### redo larger size
ggplot(df_long %>% dplyr::count(Dimension, Score) %>%    # Group by dimension and score, then count number in each group
         mutate(pct=n/138,               # Calculate percent within each region
                ypos = cumsum(n) - 0.5*n),  # Calculate label positions
       aes(Dimension, n, fill=Score)) +
  theme(plot.title = element_text(size = 22),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18), 
        legend.text=element_text(size=14), 
        legend.title = element_text(size = 14)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position=position_stack(vjust=0.5), size = 6) +
  ggtitle("DASS Domain Subscores") +
  xlab("Domain") + ylab("Percentage of cohort") +
  labs(fill = "Subscore") +
  scale_x_discrete(labels=c("D_dass_score" = "Depression", "A_dass_score" = "Anxiety", "S_dass_score" = "Stress")) 




#### and for the CBI
df_long <- gather(tp2, Dimension, Score, c(54,56,58), factor_key = TRUE)


df_long$Score <- factor(df_long$Score, levels = c("Severe","High", "Moderate", "Low"))

ggplot(df_long %>% dplyr::count(Dimension, Score) %>%    # Group by dimension and score, then count number in each group
                mutate(pct=n/138,               # Calculate percent within each region
                ypos = cumsum(n) - 0.5*n),  # Calculate label positions
       aes(Dimension, n, fill=Score)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), ## positions the percent labels
            position=position_stack(vjust=0.5))


ggplot(df_long %>% dplyr::count(Dimension, Score) %>%    # Group by dimension and score, then count number in each group
         mutate(pct=n/138,               # Calculate percent within each region
                ypos = cumsum(n) - 0.5*n),  # Calculate label positions
       aes(Dimension, n, fill=Score)) +
  theme(plot.title = element_text(size = 18),axis.text=element_text(size=12),axis.title=element_text(size=14), legend.text=element_text(size=10)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  #geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position=position_stack(vjust=0.5)) +
  ggtitle("CBI Domain Subscores") +
  xlab("Domain") + ylab("Percentage of cohort") +
  labs(fill = "Subscore", tag = "B") +
  scale_x_discrete(labels=c("P_cbi_score" = "Personal-related", "W_cbi_score" = "Workplace-related", "C_cbi_score" = "Client-related")) 



x <- df_long %>% dplyr::count(Dimension, Score) %>%    # Group by dimension and score, then count number in each group
  mutate(pct=(n/134)*100,               # Calculate percent within each region
         ypos = cumsum(n) - 0.5*n)

x$offset = c(-0.3, 0.4,0,0,0.3,0,0,-0.3,0.4,0)

x$offset = c(0.4,0,0,0.3,0,0,-0.3,0.4,0)

## larger text size
ggplot(data = x,
       aes(Dimension, n, fill=Score)) +
  theme(plot.title = element_text(size = 22),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18), 
        legend.text=element_text(size=14), 
        legend.title = element_text(size=14)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")), 
            position=position_stack(vjust=0.5), 
            size = 6, 
            vjust = x$offset) +
  ggtitle("CBI Domain Subscores") +
  xlab("Domain") + ylab("Percentage of cohort") +
  labs(fill = "Subscore") +
  scale_x_discrete(labels=c("P_cbi_score" = "Personal-related", "W_cbi_score" = "Workplace-related", "C_cbi_score" = "Client-related")) 


# 
##  [1] "#9E0142" "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#E6F598"
##  [8] "#ABDDA4" "#66C2A5" "#3288BD" "#5E4FA2"


#### A-DASS by vulnerability
my_comparisons <- list( c("Yes", "No"), c("Yes", "Unsure"), c("No", "Unsure") )
ggplot(tp2, aes(x  = Vulnerable, y = A_dass_val, fill = Vulnerable)) + 
  geom_boxplot() + 
  #stat_compare_means(aes(group = Vulnerable),label = "p.signif", label.y = c(40, 40, 40))
  stat_compare_means(comparisons = my_comparisons,label = "p.signif")

my_comparisons <- list( c("Yes", "No"), c("Yes", "Unsure"), c("No", "Unsure") )
ggplot(tp2, aes(x  = Vulnerable, y = P_cbi_val, fill = Vulnerable)) + 
  geom_boxplot() + 
  #stat_compare_means(aes(group = Vulnerable),label = "p.signif", label.y = c(40, 40, 40))
  stat_compare_means(comparisons = my_comparisons,label = "p.signif")




plot <- ggplot(tp2,aes(x = Vulnerable, y = Number_tests)) +
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=1.2)
plot


plot <- ggplot(tp2,aes(x = Vulnerable, y = Number_tests)) +
  geom_boxplot()
plot

plot <- ggplot(tp2,aes(x = Anxiety_jan, y = Days_absent)) +
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.5, dotsize=1.2)
plot

### one sample proportion test
pwr.p.test(tp2$Vulnerable)
prop.test(33, 138, p = 0.1, alternative = "two.sided",
          correct = TRUE)


prop.test(91, 138, p = NULL, alternative = "two.sided",
          correct = TRUE)

count(tp2$C_cbi_score)

cor(tp2$Anxiety_jan, tp2$A_dass_val)
ggscatter(tp2, x = "Anxiety_jan", y = "A_dass_val", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")

ggscatter(tp2, x = "Anxiety_mar", y = "A_dass_val", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")




median(tp2$Days_absent)
IQR(tp2$Days_absent)

median(tp2$Number_tests)
IQR(tp2$Number_tests)
