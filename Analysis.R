library(Hmisc)
library(tidyr)
library(randomForest)
library(pROC)
library(dplyr)
library(readr)
library(caret)
library(DMwR)


ncs <- spss.get("./data/DS0002/20240-0002-Data.sav", use.value.labels=TRUE)

# Split on variables of interest (uncombined)
vars <- c("SD2", "SD15", "SD6", "SD10A", "SD19", "SD23A", "DSM.PDS", "PD.OND", "DSM.AGO", "AGO.OND", "DSM.SP", "SP.OND", "DSM.SO", "SO.OND", "DSM.PTS", "PTS.OND", "DSM.GAD", "GAD.OND", "DSM.MDDH", "MDDH.OND", "DSM.ALA", "ALA.OND", "DSM.ALD", "ALD.OND", "DSM.DRA", "DRA.OND", "DSM.DRD", "DRD.OND", "NCSRWTLG")
ncs.sub <- ncs[vars]

# Suicide Attempt combined
ncs.sub$attempt <- NA
ncs.sub$attempt[ncs.sub$SD2=="NO" | ncs.sub$SD15=="NO" | ncs.sub$SD6=="NO" | ncs.sub$SD19=="NO"] <-  0 
ncs.sub$attempt[ncs.sub$SD6=="YES" | ncs.sub$SD19=="YES"] <- 1

# Date of last suicide attempt combination
ncs.sub$last.attempt <- NA
ncs.sub$last.attempt <- ifelse(is.na(ncs.sub$SD10A), ncs.sub$SD23A, ncs.sub$SD10A)

# Re-split on variables of interest (combined), discard uncombined
vars <- c("attempt", "last.attempt", "DSM.PDS", "PD.OND", "DSM.AGO", "AGO.OND", "DSM.SP", "SP.OND", "DSM.SO", "SO.OND", "DSM.PTS", "PTS.OND", "DSM.GAD", "GAD.OND", "DSM.MDDH", "MDDH.OND", "DSM.ALA", "ALA.OND", "DSM.ALD", "ALD.OND", "DSM.DRA", "DRA.OND", "DSM.DRD", "DRD.OND", "NCSRWTLG")
ncs.sub <- ncs.sub[vars]

# Make non-combined variable names usable  
colnames(ncs.sub) <- c("attempt", "last.attempt", "panic", "panic.onset", "agoraphobia", "agoraphobia.onset", "specific.phobia", "specific.phobia.onset", "social.phobia", "social.phobia.onset", "ptsd", "ptsd.onset", "gad", "gad.onset", "mdd", "mdd.onset", "alcohol.abuse", "alcohol.abuse.onset", "alcohol.dep", "alcohol.dep.onset", "drug.abuse", "drug.abuse.onset", "drug.dep", "drug.dep.onset", "weight")

# Recode non-numeric input and weight variables
ncs.sub <- ncs.sub %>%
  mutate_at(c("panic", "agoraphobia", "specific.phobia", "social.phobia", "ptsd", "gad", "mdd", "alcohol.abuse", "alcohol.dep", "drug.abuse", "drug.dep"), funs(recode(., `ENDORSED`=1, `NOT ENDORSED`=0, .default=NaN)))
ncs.sub$weight <- ifelse(is.na(ncs.sub$weight), 1, ncs.sub$weight)

# Factor non-numeric variables
cols <- c("attempt", "panic", "agoraphobia", "specific.phobia", "social.phobia", "ptsd", "gad", "mdd", "alcohol.abuse", "alcohol.dep", "drug.abuse", "drug.dep") # Holding for next line
ncs.sub[cols] <- lapply(ncs.sub[cols], factor)
ncs.sub$attempt <- factor(ncs.sub$attempt)

# Numeric/continuous variables -- recode less than 4
ncs.sub$panic.onset <- as.character(ncs.sub$panic.onset)
ncs.sub$panic.onset[ncs.sub$panic.onset=="4 OR LESS"] <- "4"
ncs.sub$agoraphobia.onset <- as.character(ncs.sub$agoraphobia.onset)
ncs.sub$agoraphobia.onset[ncs.sub$agoraphobia.onset=="4 OR LESS"] <- "4"
ncs.sub$specific.phobia.onset <- as.character(ncs.sub$specific.phobia.onset)
ncs.sub$specific.phobia.onset[ncs.sub$specific.phobia.onset=="4 OR LESS"] <- "4"
ncs.sub$social.phobia.onset <- as.character(ncs.sub$social.phobia.onset)
ncs.sub$social.phobia.onset[ncs.sub$social.phobia.onset=="4 OR LESS"] <- "4"
ncs.sub$ptsd.onset <- as.character(ncs.sub$ptsd.onset)
ncs.sub$ptsd.onset[ncs.sub$ptsd.onset=="4 OR LESS"] <- "4"
ncs.sub$gad.onset <- as.character(ncs.sub$gad.onset)
ncs.sub$gad.onset[ncs.sub$gad.onset=="4 OR LESS"] <- "4"
ncs.sub$mdd.onset <- as.character(ncs.sub$mdd.onset)
ncs.sub$mdd.onset[ncs.sub$mdd.onset=="4 OR LESS"] <- "4"
ncs.sub$alcohol.abuse.onset <- as.character(ncs.sub$alcohol.abuse.onset)
ncs.sub$alcohol.abuse.onset[ncs.sub$alcohol.abuse.onset=="4 OR LESS"] <- "4"
ncs.sub$alcohol.dep.onset <- as.character(ncs.sub$alcohol.dep.onset)
ncs.sub$alcohol.dep.onset[ncs.sub$alcohol.dep.onset=="4 OR LESS"] <- "4"
ncs.sub$drug.abuse.onset <- as.character(ncs.sub$drug.abuse.onset)
ncs.sub$drug.abuse.onset[ncs.sub$drug.abuse.onset=="4 OR LESS"] <- "4"
ncs.sub$drug.dep.onset <- as.character(ncs.sub$drug.dep.onset)
ncs.sub$drug.dep.onset[ncs.sub$drug.dep.onset=="4 OR LESS"] <- "4"

# Numeric these recoded numeric variables + weights
cols <- c("last.attempt", "panic.onset", "agoraphobia.onset", "specific.phobia.onset", "social.phobia.onset", "ptsd.onset", "gad.onset", "mdd.onset", "alcohol.abuse.onset", "alcohol.dep.onset", "drug.abuse.onset", "drug.dep.onset", "weight")
ncs.sub[cols] <- lapply(ncs.sub[cols], as.numeric)

# Remove people with missing data on suicide attempts
length(which(ncs.sub$attempt==0 & !is.na(ncs.sub$last.attempt))) # -- Check for people who DIDN'T attempt suicide but DO have an age of last attempt. None.
length(which(ncs.sub$attempt==1 & is.na(ncs.sub$last.attempt))) # -- Check for people who DID attempt but DON'T have an age of last attempt. 58.

# Remove those with missing data on suicide attempts 
ncs.sub <- ncs.sub %>% filter(!is.na(attempt))

# Remove those who have attempted suicide but don't have age data
ncs.sub <- ncs.sub[!(ncs.sub$attempt==1 & is.na(ncs.sub$last.attempt)),] # -58 observations

# Remove those people with missing data on any mental disorder
ncs.sub <- ncs.sub %>% drop_na(panic, agoraphobia, specific.phobia, social.phobia, ptsd, gad, mdd, alcohol.abuse, alcohol.dep, drug.abuse, drug.dep) # -0 observations

# Remove those people with disorder but no age data
ncs.sub <- ncs.sub[!(ncs.sub$panic==1 & is.na(ncs.sub$panic.onset)),] # -0
ncs.sub <- ncs.sub[!(ncs.sub$agoraphobia==1 & is.na(ncs.sub$agoraphobia.onset)),] # -0
ncs.sub <- ncs.sub[!(ncs.sub$specific.phobia==1 & is.na(ncs.sub$specific.phobia.onset)),] # -0
ncs.sub <- ncs.sub[!(ncs.sub$social.phobia==1 & is.na(ncs.sub$social.phobia.onset)),] # -0
ncs.sub <- ncs.sub[!(ncs.sub$ptsd == 1 & is.na(ncs.sub$ptsd.onset)),] # -0
ncs.sub <- ncs.sub[!(ncs.sub$gad == 1 & is.na(ncs.sub$gad.onset)),] # -0
ncs.sub <- ncs.sub[!(ncs.sub$mdd == 1 & is.na(ncs.sub$mdd.onset)),] # -0
ncs.sub <- ncs.sub[!(ncs.sub$alcohol.abuse == 1 & is.na(ncs.sub$alcohol.abuse.onset)),] # -0
ncs.sub <- ncs.sub[!(ncs.sub$alcohol.dep == 1 & is.na(ncs.sub$alcohol.dep.onset)),] # -0
ncs.sub <- ncs.sub[!(ncs.sub$drug.abuse == 1 & is.na(ncs.sub$drug.abuse.onset)),] # -0
ncs.sub <- ncs.sub[!(ncs.sub$drug.dep==1 & is.na(ncs.sub$drug.dep.onset)),]

# New Variables coded to show whether disorder onset preceeded suicide attempt
ncs.sub$panic1 <- ifelse(ncs.sub$panic==1 & ((ncs.sub$panic.onset <= ncs.sub$last.attempt) | (ncs.sub$attempt==0)), 1, 0)
ncs.sub$agoraphobia1 <- ifelse(ncs.sub$agoraphobia==1 & ((ncs.sub$agoraphobia.onset <= ncs.sub$last.attempt) | (ncs.sub$attempt==0)), 1, 0)
ncs.sub$specific.phobia1 <- ifelse(ncs.sub$specific.phobia==1 & ((ncs.sub$specific.phobia.onset <= ncs.sub$last.attempt) | (ncs.sub$attempt==0)), 1, 0)
ncs.sub$social.phobia1 <- ifelse(ncs.sub$social.phobia==1 & ((ncs.sub$social.phobia.onset <= ncs.sub$last.attempt) | (ncs.sub$attempt==0)), 1, 0)
ncs.sub$ptsd1 <- ifelse(ncs.sub$ptsd==1 & ((ncs.sub$ptsd.onset <= ncs.sub$last.attempt) | (ncs.sub$attempt==0)), 1, 0)
ncs.sub$gad1 <- ifelse(ncs.sub$gad==1 & ((ncs.sub$gad.onset <= ncs.sub$last.attempt) | (ncs.sub$attempt==0)), 1, 0)
ncs.sub$mdd1 <- ifelse(ncs.sub$mdd==1 & ((ncs.sub$mdd.onset <= ncs.sub$last.attempt) | (ncs.sub$attempt==0)), 1, 0)
ncs.sub$alcohol.abuse1 <- ifelse(ncs.sub$alcohol.abuse==1 & ((ncs.sub$alcohol.abuse.onset <= ncs.sub$last.attempt) | (ncs.sub$attempt==0)), 1, 0)
ncs.sub$alcohol.dep1 <- ifelse(ncs.sub$alcohol.dep==1 & ((ncs.sub$alcohol.dep.onset <= ncs.sub$last.attempt) | (ncs.sub$attempt==0)), 1, 0)
ncs.sub$drug.abuse1 <- ifelse(ncs.sub$drug.abuse==1 & ((ncs.sub$drug.abuse.onset <= ncs.sub$last.attempt) | (ncs.sub$attempt==0)), 1, 0)
ncs.sub$drug.dep1 <- ifelse(ncs.sub$drug.dep==1 & ((ncs.sub$drug.dep.onset <= ncs.sub$last.attempt) | (ncs.sub$attempt==0)), 1, 0)

# Pull final variables for analysis
vars <- c("panic1", "agoraphobia1", "specific.phobia1", "social.phobia1", "ptsd1", "gad1", "mdd1", "alcohol.abuse1", "alcohol.dep1", "drug.abuse1", "drug.dep1", "attempt", "weight")
ncs.final <- ncs.sub[vars]
vars <- c("panic1", "agoraphobia1", "specific.phobia1", "social.phobia1", "ptsd1", "gad1", "mdd1", "alcohol.abuse1", "alcohol.dep1", "drug.abuse1", "drug.dep1", "attempt")
ncs.final[vars] <- lapply(ncs.final[vars], factor)
ncs.final$weight <- as.numeric(ncs.final$weight)

# Training and testing sets
index <- createDataPartition(ncs.final$attempt, p = 0.8, list = FALSE)
train_data <- ncs.final[index, ]
test_data <- ncs.final[-index, ]
train_data.balanced <- SMOTE(attempt ~ panic1 + agoraphobia1 + specific.phobia1 + social.phobia1 + ptsd1 + gad1 + mdd1
                             + alcohol.abuse1 + alcohol.dep1 + drug.abuse1 + drug.dep1, train_data, k = 12, perc.over = 60, perc.under = 200)


# Run the forest
rf <- randomForest(attempt ~ panic1 + agoraphobia1 + specific.phobia1 + social.phobia1 + ptsd1 + gad1 + mdd1
             + alcohol.abuse1 + alcohol.dep1 + drug.abuse1 + drug.dep1, data = train_data.balanced, split = 'gini')
test_data$predicted.response <- predict(rf, test_data, type = "class")
cm <- confusionMatrix(test_data$predicted.response, as.factor(test_data$attempt), positive = '1')
cm





