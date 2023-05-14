### COVID MHCW
### Initial survey data, collected Jan - Feb 2021
### data prep file


#setwd
setwd("~/BioInformatics Work/Psychiatry Bioinformatics/COVID19_MHCW/COVID_MHCW/Timepoint 2")

## libraries
library(tidyverse)
library(plyr)
library(dplyr)
library(likert)
library(ggplot2)

## read in the data from timepoint 1
# make strings factors, and force empty cells into NAs
tp1 <- read.csv("CMHW_tp2.csv", header = TRUE, stringsAsFactors = TRUE, na.strings="")


#################################################
### DASS

#values 
# N=0, S=1, O=2, AA=3)
# set values for factors
## set the columns to change

cols <- tp1[,47:67]

cols <- as.data.frame(lapply(cols, function(x) { revalue(x, c("Never" = 0, "Sometimes" = 1, "Often" = 2, "Nearly always" = 3)) }))

write.table(cols, "DASS_converted.txt", sep = "\t")


cols <- read.table("DASS_converted.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
## set the depression domain questions
### Depression questions
d_dass <- c("D1","D2","D4","D6","D7","D8","D9","D11","D12","D14","D15","D18","D19","D20")


#get sum of this and add as new column
cols$d_dass_val <- rowSums(cols[,d_dass])

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

write.table(cols, "Dass_outcomes_TP2.txt", sep = "\t")

##########
############################################
## CBI


cols <- tp1[,68:86]

cols <- as.data.frame(lapply(cols, function(x) { revalue(x, c("Always" = 100, "Often" = 75, "Sometimes" = 50, "Seldom" = 25, "Never" = 0)) }))

write.table(cols, "CBI_converted.txt", sep = "\t")

cols <- read.table("CBI_converted.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
#question 10 is inverted:
cols <-  revalue(cols$C10, c("100" = 0, "75" = 25, "25"=75, "0" =100))

cols_test <- cols

cols_test$C10 <- dplyr::recode(cols$C10, `100` = 0, `75` = 25, `25` = 75, `0` = 100 , `50` = 50)

write.table(cols_test, "CBI_converted.txt", sep = "\t")

#df <- read.csv("Book7.csv", header = TRUE)
#df2 <- as.data.frame(lapply(df, function(x) { revalue(x, c("Always" = 100, "Often" = 75, "Sometimes" = 50, "Seldom" = 25, "Never" = 0)) }))
#write.table(df2, "extra_cbi.txt", sep = "\t")

#personal burnout
p_cbi <- c("C1", "C2", "C3", "C4", "C5", "C6")
cols$p_cbi_val <- (rowSums(cols[,p_cbi]))/6


#scoring
# score based on the values, and create column for this 
#personal

p_cbi_fun <- function(df) {
  for (i in 1:nrow(df)) {
    
    df[i,"P_cbi_score"] <- ifelse(df[i,"p_cbi_val"] <= 50, "Low", 
                                   ifelse(df[i,"p_cbi_val"] <= 74, "Moderate",
                                          ifelse(df[i,"p_cbi_val"]<= 99, "High",
                                                 ifelse(df[i,"p_cbi_val"] <= 100, "Severe"))))
    
  }
  return(df)
}
###
cols <- p_cbi_fun(cols)

# work related burnout
w_cbi <- c("C7", "C8", "C9", "C10", "C11", "C12", "C13")
cols$w_cbi_val <- (rowSums(cols[,w_cbi]))/7


#scoring
# score based on the values, and create column for this 

w_cbi_fun <- function(df) {
  for (i in 1:nrow(df)) {
    
    df[i,"w_cbi_score"] <- ifelse(df[i,"w_cbi_val"] <= 50, "Low", 
                                  ifelse(df[i,"w_cbi_val"] <= 74, "Moderate",
                                         ifelse(df[i,"w_cbi_val"]<= 99, "High",
                                                ifelse(df[i,"w_cbi_val"] <= 100, "Severe"))))
    
  }
  return(df)
}
###
cols <- w_cbi_fun(cols)

# client related burnout
c_cbi <- c("C14", "C15", "C16", "C17", "C18", "C19")
cols$c_cbi_val <- (rowSums(cols[,c_cbi]))/6


#scoring
# score based on the values, and create column for this 

c_cbi_fun <- function(df) {
  for (i in 1:nrow(df)) {
    
    df[i,"c_cbi_score"] <- ifelse(df[i,"c_cbi_val"] <= 50, "Low", 
                                  ifelse(df[i,"c_cbi_val"] <= 74, "Moderate",
                                         ifelse(df[i,"c_cbi_val"]<= 99, "High",
                                                ifelse(df[i,"c_cbi_val"] <= 100, "Severe"))))
    
  }
  return(df)
}
###
cols <- c_cbi_fun(cols)

## 
write.table(cols, "CBI_Outcomes_TP2.txt", sep = "\t")
  

