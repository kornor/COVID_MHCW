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
## read in the data from timepoint 1
# make strings factors, and force empty cells into NAs
tp1 <- read.csv("Data_tp1_cleaned_v2.csv", header = TRUE, stringsAsFactors = TRUE, na.strings="")
##and timepoint 2
# make strings factors, and force empty cells into NAs
tp2 <- read.csv("Data_tp2_cleaned.csv", header = TRUE, stringsAsFactors = TRUE, na.strings="")


## subset the matched ones

trim1 <- tp1 %>% filter (tp1$UnID %in% tp2$UnID)
trim2 <- trim2 %>% filter (trim2$UnID %in% trim1$UnID)

write.csv(trim1, "Subset_timepoint1.csv")
write.csv(trim2, "Subset_timepoint2.csv")


trim1 <- read.csv("Subset_timepoint1.csv", header = TRUE, stringsAsFactors = TRUE, na.strings="")
##and timepoint 2
# make strings factors, and force empty cells into NAs
trim2 <- read.csv("Subset_timepoint2.csv", header = TRUE, stringsAsFactors = TRUE, na.strings="")


sub <- read.csv("Combo_subset.csv", header = TRUE, stringsAsFactors = TRUE, na.strings="")

## no correlation between self reproted anxiety and historically recognised anxiety 
ggplot(sub, aes(x  = Anxiety_2, y = Anxiety_3)) + geom_point()
cor.test(as.numeric(sub$Anxiety_2), as.numeric(sub$Anxiety_3))

## however strong correlation between past a dass scpre and current
ggplot(sub, aes(x  = A_dass_val, y = A_dass_val_2)) + geom_point()
cor.test(as.numeric(sub$A_dass_val), as.numeric(sub$A_dass_val_2))

cor.test(as.numeric(sub$A_dass_val), as.numeric(sub$Anxiety_2))
cor.test(as.numeric(sub$A_dass_val_2), as.numeric(sub$Anxiety_4))


# pivot / gather
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long2 <- gather(sub, Timepoint, A_Dass, A_dass_val:A_dass_val_2, factor_key=TRUE)



# "p.signif" is stars
# p.format = the value
# no comment, the test and the value
my_comparisons <- list( c("Yes", "No"), c("Yes", "Unsure"), c("No", "Unsure") )
my_comparisons <- list(c("A_dass_val", "A_dass_val_2"))
levels(data_long2$Timepoint)
data_long2$Timepoint <- fct_rev(data_long2$Timepoint)

p <- 
  ggplot(data_long2, aes(x=Timepoint, y=A_Dass, fill = Vulnerable)) + 
  geom_boxplot() +
  stat_compare_means(comparisons =  my_comparisons, label = "p.format",label.y = 40, size = 6) +
  stat_compare_means(aes(group = Vulnerable),label = "p.format", label.y = c(35, 35), size = 6) +
  ggtitle("DASS Anxiety Subscore \nby identification as vulnerable employee") +
  xlab("Timepoint") + ylab("DASS Anxiety Subscore") +
  labs(fill = "Respondant identifies \nas vulnerable employee") +
  scale_x_discrete(labels=c("A_dass_val" = "January 2021", "A_dass_val_2" = "January 2022")) +
  theme(legend.position = "none", text = element_text(size=20))

p




# "p.signif" is stars
# p.format = the value
# no comment, the test and the value
my_comparisons <- list( c("Yes", "No"), c("Yes", "Unsure"), c("No", "Unsure") )
my_comparisons <- list(c("D_dass_val", "A_dass_val_2"))
levels(data_long2$Timepoint)
data_long2$Timepoint <- fct_rev(data_long2$Timepoint)

p <- 
  ggplot(data_long2, aes(x=Timepoint, y=A_Dass, fill = Vulnerable)) + 
  geom_boxplot() +
  stat_compare_means(comparisons =  my_comparisons, label = "p.format",label.y = 40, size = 6) +
  stat_compare_means(aes(group = Vulnerable),label = "p.format", label.y = c(35, 35), size = 6) +
  ggtitle("DASS Anxiety Subscore \nby identification as vulnerable employee") +
  xlab("Timepoint") + ylab("DASS Anxiety Subscore") +
  labs(fill = "Respondant identifies \nas vulnerable employee") +
  scale_x_discrete(labels=c("A_dass_val" = "January 2021", "A_dass_val_2" = "January 2022")) +
  theme(legend.position = "none", text = element_text(size=20))

p




ggplot((x  = A_dass_score, y = Anxiety.Now)) + geom_boxplot() + stat_compare_means()




t.test(sub$A_dass_val, sub$A_dass_val_2, paired = TRUE, alternative = "two.sided")
## no difference!



combo_all <- read.csv("Combo_times.csv", header = TRUE, stringsAsFactors = TRUE)
combo_sub <- read.csv("Combo_subset_2.csv", header = TRUE, stringsAsFactors = TRUE)

combo_all$Timepoint <- as.factor(combo_all$Timepoint)
combo_sub$Timepoint <- as.factor(combo_sub$Timepoint)

ggplot(combo_all, aes(x  = Timepoint, y = Value, fill = Dimension)) + geom_boxplot() 

levels(combo_all$Dimension)
data_long$Timepoint <- (data_long$Timepoint)
combo_all$Dimension <- factor(combo_all$Dimension, levels = c("D_DASS", "A_DASS", "S_DASS", "P_CBI", "W_CBI", "C_CBI"))
combo_sub$Dimension <- factor(combo_sub$Dimension, levels = c("D_DASS", "A_DASS", "S_DASS", "P_CBI", "W_CBI", "C_CBI"))

 combo_all_1 <- subset(combo_all, filter (Dimension = c("D_DASS", "A_DASS", "S_DASS"))

ggplot(combo_all, aes(x  = Timepoint, y = Value)) + 
  geom_boxplot() + 
  facet_wrap(~Dimension) + 
  stat_compare_means()

## matched
ggplot(combo_sub, aes(x  = Timepoint, y = Value)) + 
  geom_boxplot() + 
  facet_wrap(~Dimension) + 
  stat_compare_means()
