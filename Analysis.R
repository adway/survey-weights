library(Hmisc)
library(tidyr)
library(ranger)
library(pROC)
library(dplyr)
library(readr)

ncs <- spss.get("./data/DS0002/20240-0002-Data.sav", use.value.labels=TRUE)

# Split on variables of interest (uncombined)
vars <- c("SD2", "SD15", "SD6", "SD10A", "SD19", "SD23A", "DSM.PDS", "PD.OND", "DSM.AGO", "AGO.OND", "DSM.SP", "SP.OND", "DSM.SO", "SO.OND", "DSM.PTS", "PTS.OND", "DSM.GAD", "GAD.OND", "DSM.MDDH", "MDDH.OND", "DSM.ALA", "ALA.OND", "DSM.ALD", "ALD.OND", "DSM.DRA", "DRA.OND", "DSM.DRD", "DRD.OND", "NCSRWTLG")
ncs.sub <- ncs[vars]

# Suicide Attempt combined
ncs.sub$attempt <- NA
ncs.sub$attempt[ncs.sub$SD2=="NO" | ncs.sub$SD15=="NO" | ncs.sub$SD5=="NO" | ncs.sub$SD19=="NO"] <- 0
ncs.sub$attempt[ncs.sub$SD5=="YES" | ncs.sub$SD19=="YES"] <- 1

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















