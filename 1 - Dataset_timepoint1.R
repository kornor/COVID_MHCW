### COVID MHCW
### Initial survey data, collected Jan - Feb 2021
### data prep file


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




#################################################
### DASS

#values 
# N=0, S=1, O=2, AA=3)
# set values for factors
## set the columns to change

cols <- tp1[,28:48]

#cols <- as.data.frame(lapply(cols, function(x) { revalue(x, c("Never" = 0, "Sometimes" = 1, "Often" = 2, "Nearly always" = 3)) }))
#write.table(cols, "DASS_converted.txt", sep = "\t")

## set the depression domain questions
### Depression questions
d_dass <- c("D1","D2","D4","D6","D7","D8","D9","D11","D12","D14","D15","D18","D19","D20")
#get sum of this and add as new column
cols$d_dass_val <- rowSums(cols_test[,d_dass])

#scoring
# score based on the values, and create column for this 
#depression

d_dass_fun <- function(df) {
  for (i in 1:nrow(df)) {
    
    df[i,"D_dass_score"] <- ifelse(df[i,"d_dass_val"] <= 4, "Normal", 
                                   ifelse(df[i,"d_dass_val"] <= 6, "Mild",
                                          ifelse(df[i,"d_dass_val"]<= 10, "Moderate",
                                                 ifelse(df[i,"d_dass_val"] <= 13, "Severe", 
                                                        ifelse(df[i,"d_dass_val"] >=14, "Extremely severe", "NA")))))
 
  }
  return(df)
}

#############################
cols <- d_dass_fun(cols)


################################################
#anxiety
a_dass <- c("D1","D3","D5","D6","D8","D10","D11","D12","D13","D14","D16","D17","D18","D21")
cols$a_dass_val <- rowSums(cols[,a_dass])

#anxiety
a_dass_fun <- function(df) {
  for (i in 1:nrow(df)) {
    
    df[i,"A_dass_score"] <- ifelse(df[i,"a_dass_val"] <= 3, "Normal", 
                                   ifelse(df[i,"a_dass_val"] <= 5, "Mild",
                                          ifelse(df[i,"a_dass_val"]<= 7, "Moderate",
                                                 ifelse(df[i,"a_dass_val"] <= 9, "Severe", 
                                                        ifelse(df[i,"a_dass_val"] >=10, "Extremely severe", "NA")))))
    
  }
  return(df)
}

### 
cols <- a_dass_fun(cols)

#stress
#stress
s_dass <-c("D2","D3","D4","D5","D7","D9","D10","D13","D15","D16","D17","D19","D20","D21")

cols$s_dass_val <- rowSums(cols[,s_dass])


##
s_dass_fun <- function(df) {
  for (i in 1:nrow(df)) {
    
    df[i,"S_dass_score"] <- ifelse(df[i,"s_dass_val"] <= 7, "Normal", 
                                   ifelse(df[i,"s_dass_val"] <= 9, "Mild",
                                          ifelse(df[i,"s_dass_val"]<= 12, "Moderate",
                                                 ifelse(df[i,"s_dass_val"] <= 16, "Severe", 
                                                        ifelse(df[i,"s_dass_val"] >=17, "Extremely severe", "NA")))))
    
  }
  return(df)
}

#### 
cols <- s_dass_fun(cols)

write.table(cols, "Dass_outcomes_TP1.txt", sep = "\t")

##########

