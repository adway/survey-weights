library(tidyverse)
library(broom)
library(caret)
library(survey)
library(DMwR)
library(randomForest)
library(pracma)
library(pROC)
library(here)
library(Hmisc)
source("weighted-metrics.R")

argv <- commandArgs(TRUE)
if (length(argv) > 0){
  trial <- as.numeric( argv[1] )
} else {
  stop("Please input parameters")
}

ncs <- spss.get(here("data/ncsr-data.sav"), use.value.labels=TRUE)

####################################################################################################################################
### Subsetting data and recoding variables  
####################################################################################################################################
#Subset data for only variables of interest
vars <- c("SD2", "SD15", "SD6", "SD10A", "SD19", "SD23A", "DSM.PDS", "PD.OND", "DSM.AGO", "AGO.OND", "DSM.SP", "SP.OND", "DSM.SO", "SO.OND", "DSM.PTS", "PTS.OND", "DSM.GAD", "GAD.OND", "DSM.MDDH", "MDDH.OND", "DSM.ALA", "ALA.OND", "DSM.ALD", "ALD.OND", "DSM.DRA", "DRA.OND", "DSM.DRD", "DRD.OND", "NCSRWTSH")
ncs.sub <- ncs[vars]

#Create new suicide attempt variable
ncs.sub$attempt <- NA
ncs.sub$attempt[ncs.sub$SD2=="NO" | ncs.sub$SD15=="NO" | ncs.sub$SD6=="NO" | ncs.sub$SD19=="NO"] <-  0 
ncs.sub$attempt[ncs.sub$SD6=="YES" | ncs.sub$SD19=="YES"] <- 1


#Create a new date of last suicide attempt variable 
ncs.sub$last.attempt <- NA
ncs.sub$last.attempt <- ifelse(!is.na(ncs.sub$SD10A), ncs.sub$SD10A, ncs.sub$SD23A)


#Subset data and rename variables
vars <- c("attempt", "last.attempt", "DSM.PDS", "PD.OND", "DSM.AGO", "AGO.OND", "DSM.SP", "SP.OND", "DSM.SO", "SO.OND", "DSM.PTS", "PTS.OND", "DSM.GAD", "GAD.OND", "DSM.MDDH", "MDDH.OND", "DSM.ALA", "ALA.OND", "DSM.ALD", "ALD.OND", "DSM.DRA", "DRA.OND", "DSM.DRD", "DRD.OND", "NCSRWTSH")
ncs.sub <- ncs.sub[vars]
colnames(ncs.sub) <- c("attempt", "last.attempt", "panic", "panic.onset", "agoraphobia", "agoraphobia.onset", "specific.phobia", "specific.phobia.onset", "social.phobia", "social.phobia.onset", "ptsd", "ptsd.onset", "gad", "gad.onset", "mdd", "mdd.onset", "alcohol.abuse", "alcohol.abuse.onset", "alcohol.dep", "alcohol.dep.onset", "drug.abuse", "drug.abuse.onset", "drug.dep", "drug.dep.onset", "Weight") 


#Recode variables
ncs.sub1 <- ncs.sub %>% 
  mutate_at(c("panic", "agoraphobia", "specific.phobia", "social.phobia", "ptsd", "gad", "mdd", "alcohol.abuse", "alcohol.dep", "drug.abuse", "drug.dep"), funs(recode(., `ENDORSED`=1, `NOT ENDORSED`=0, .default = NaN)))

ncs.sub1$attempt <- factor(ncs.sub1$attempt)


#Factor variables
cols <- c("attempt", "panic", "agoraphobia", "specific.phobia", "social.phobia", "ptsd", "gad", "mdd", "alcohol.abuse", "alcohol.dep", "drug.abuse", "drug.dep")
ncs.sub1[cols] <- lapply(ncs.sub1[cols], factor)


#Numeric variables
ncs.sub1$panic.onset <- as.character(ncs.sub1$panic.onset)
ncs.sub1$panic.onset[ncs.sub1$panic.onset=="4 OR LESS"] <- "4"

ncs.sub1$agoraphobia.onset <- as.character(ncs.sub1$agoraphobia.onset)
ncs.sub1$agoraphobia.onset[ncs.sub1$agoraphobia.onset=="4 OR LESS"] <- "4"

ncs.sub1$specific.phobia.onset <- as.character(ncs.sub1$specific.phobia.onset)
ncs.sub1$specific.phobia.onset[ncs.sub1$specific.phobia.onset=="4 OR LESS"] <- "4"

ncs.sub1$social.phobia.onset <- as.character(ncs.sub1$social.phobia.onset)
ncs.sub1$social.phobia.onset[ncs.sub1$social.phobia.onset=="4 OR LESS"] <- "4"

ncs.sub1$ptsd.onset <- as.character(ncs.sub1$ptsd.onset)
ncs.sub1$ptsd.onset[ncs.sub1$ptsd.onset=="4 OR LESS"] <- "4"

ncs.sub1$gad.onset <- as.character(ncs.sub1$gad.onset)
ncs.sub1$gad.onset[ncs.sub1$gad.onset=="4 OR LESS"] <- "4"

ncs.sub1$mdd.onset <- as.character(ncs.sub1$mdd.onset)
ncs.sub1$mdd.onset[ncs.sub1$mdd.onset=="4 OR LESS"] <- "4"

ncs.sub1$alcohol.abuse.onset <- as.character(ncs.sub1$alcohol.abuse.onset)
ncs.sub1$alcohol.abuse.onset[ncs.sub1$alcohol.abuse.onset=="4 OR LESS"] <- "4"

ncs.sub1$alcohol.dep.onset <- as.character(ncs.sub1$alcohol.dep.onset)
ncs.sub1$alcohol.dep.onset[ncs.sub1$alcohol.dep.onset=="4 OR LESS"] <- "4"

ncs.sub1$drug.abuse.onset <- as.character(ncs.sub1$drug.abuse.onset)
ncs.sub1$drug.abuse.onset[ncs.sub1$drug.abuse.onset=="4 OR LESS"] <- "4"

ncs.sub1$drug.dep.onset <- as.character(ncs.sub1$drug.dep.onset)
ncs.sub1$drug.dep.onset[ncs.sub1$drug.dep.onset=="4 OR LESS"] <- "4"

cols2 <- c("last.attempt", "panic.onset", "agoraphobia.onset", "specific.phobia.onset", "social.phobia.onset", "ptsd.onset", "gad.onset", "mdd.onset", "alcohol.abuse.onset", "alcohol.dep.onset", "drug.abuse.onset", "drug.dep.onset")
ncs.sub1[cols2] <- lapply(ncs.sub1[cols2], as.numeric)


#Remove persons missing data on suicide attempts
#length(which(ncs.sub1$attempt==0 & ncs.sub1$last.attempt>0)) #Check to see if there's anyone who did not endorse a suicide attempt but had an age of last attempt. There were none.

ncs.sub2 <- ncs.sub1 %>% filter(is.na(attempt)==F) #people retained after removing persons missing data on suicide attempts


#Remove persons who attempted suicide but were missing data on age at last suicide attempt
#length(which(ncs.sub2$attempt==1 & is.na(ncs.sub2$last.attempt)==T))

ncs.sub3 <- ncs.sub2[!(ncs.sub2$attempt==1 & is.na(ncs.sub2$last.attempt)==T),]


#Remove persons missing data on any of the mental disorders
ncs.sub4 <- ncs.sub3 %>% drop_na(panic, agoraphobia, specific.phobia, social.phobia, ptsd, gad, mdd, alcohol.abuse, alcohol.dep, drug.abuse, drug.dep) #none were missing


#Remove persons with mental disorder diagnoses who were missing data on age of onset. There weren't any people with a mental disorder who were missing data on age of onset.
#length(which(ncs.sub4$panic==1 & is.na(ncs.sub4$panic.onset)==T))
ncs.sub5 <- ncs.sub4[!(ncs.sub4$panic==1 & is.na(ncs.sub4$panic.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$agoraphobia==1 & is.na(ncs.sub5$agoraphobia.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$specific.phobia==1 & is.na(ncs.sub5$specific.phobia.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$social.phobia==1 & is.na(ncs.sub5$social.phobia.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$ptsd==1 & is.na(ncs.sub5$ptsd.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$gad==1 & is.na(ncs.sub5$gad.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$mdd==1 & is.na(ncs.sub5$mdd.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$alcohol.abuse==1 & is.na(ncs.sub5$alcohol.abuse.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$alcohol.dep==1 & is.na(ncs.sub5$alcohol.dep.onset)==T),]
ncs.sub5 <- ncs.sub5[!(ncs.sub5$drug.abuse==1 & is.na(ncs.sub5$drug.abuse.onset)==T),]
ncs.comp <- ncs.sub5[!(ncs.sub5$drug.dep==1 & is.na(ncs.sub5$drug.dep.onset)==T),]


#Create new variables for the presence of mental disorders before or during a suicide attempt. I will use these new variables as predictors to make sure that the mental disorders preceded suicide attempts
ncs.comp$panic1 <- ifelse(ncs.comp$panic==1 & ncs.comp$panic.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$panic1[ncs.comp$panic==1 & ncs.comp$attempt==0] <- 1

ncs.comp$agoraphobia1 <- ifelse(ncs.comp$agoraphobia==1 & ncs.comp$agoraphobia.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$agoraphobia1[ncs.comp$agoraphobia==1 & ncs.comp$attempt==0] <- 1

ncs.comp$specific.phobia1 <- ifelse(ncs.comp$specific.phobia==1 & ncs.comp$specific.phobia.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$specific.phobia1[ncs.comp$specific.phobia==1 & ncs.comp$attempt==0] <- 1

ncs.comp$social.phobia1 <- ifelse(ncs.comp$social.phobia==1 & ncs.comp$social.phobia.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$social.phobia1[ncs.comp$social.phobia==1 & ncs.comp$attempt==0] <- 1

ncs.comp$ptsd1 <- ifelse(ncs.comp$ptsd==1 & ncs.comp$ptsd.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$ptsd1[ncs.comp$ptsd==1 & ncs.comp$attempt==0] <- 1

ncs.comp$gad1 <- ifelse(ncs.comp$gad==1 & ncs.comp$gad.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$gad1[ncs.comp$gad==1 & ncs.comp$attempt==0] <- 1

ncs.comp$mdd1 <- ifelse(ncs.comp$mdd==1 & ncs.comp$mdd.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$mdd1[ncs.comp$mdd==1 & ncs.comp$attempt==0] <- 1

ncs.comp$alcohol.abuse1 <- ifelse(ncs.comp$alcohol.abuse==1 & ncs.comp$alcohol.abuse.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$alcohol.abuse1[ncs.comp$alcohol.abuse==1 & ncs.comp$attempt==0] <- 1

ncs.comp$alcohol.dep1 <- ifelse(ncs.comp$alcohol.dep==1 & ncs.comp$alcohol.dep.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$alcohol.dep1[ncs.comp$alcohol.dep==1 & ncs.comp$attempt==0] <- 1

ncs.comp$drug.abuse1 <- ifelse(ncs.comp$drug.abuse==1 & ncs.comp$drug.abuse.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$drug.abuse1[ncs.comp$drug.abuse==1 & ncs.comp$attempt==0] <- 1

ncs.comp$drug.dep1 <- ifelse(ncs.comp$drug.dep==1 & ncs.comp$drug.dep.onset <= ncs.comp$last.attempt, 1, 0) 
ncs.comp$drug.dep1[ncs.comp$drug.dep==1 & ncs.comp$attempt==0] <- 1

#View data to see if I am doing what I think I'm doing:
#ncs.comp %>% filter(alcohol.abuse==1) %>% select(alcohol.abuse, alcohol.abuse.onset, alcohol.abuse1, attempt, last.attempt) 


#Subset data for only the variables I need for analysis
vars <- c("panic1", "agoraphobia1", "specific.phobia1", "social.phobia1", "ptsd1", "gad1", "mdd1", "alcohol.abuse1", "alcohol.dep1", "drug.abuse1", "drug.dep1", "attempt", "Weight")
vars.sub <- c("panic1", "agoraphobia1", "specific.phobia1", "social.phobia1", "ptsd1", "gad1", "mdd1", "alcohol.abuse1", "alcohol.dep1", "drug.abuse1", "drug.dep1", "attempt")
ncs.final <- ncs.comp[vars] 

cols <- names(ncs.final)
ncs.final[vars.sub] <- lapply(ncs.final[vars.sub], factor)
ncs.final$Weight <- lapply(ncs.final$Weight, as.numeric)
final_pop <- ncs.final

save(final_pop, file = "ncsr-with-weights.RData")
