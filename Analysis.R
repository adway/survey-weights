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















