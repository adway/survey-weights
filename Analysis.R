library(Hmisc)
library(tidyr)
library(ranger)
library(pROC)
library(dplyr)
library(readr)

ncs <- spss.get("./data/DS0002/20240-0002-Data.sav", use.value.labels=TRUE)

# Get Variables of Interest to combine
vars <- c("SD2", "SD15", "SD6", "SD10A", "SD19", "SD23A", "DSM.PDS", "PD.OND", "DSM.AGO", "AGO.OND", "DSM.SP", "SP.OND", "DSM.SO", "SO.OND", "DSM.PTS", "PTS.OND", "DSM.GAD", "GAD.OND", "DSM.MDDH", "MDDH.OND", "DSM.ALA", "ALA.OND", "DSM.ALD", "ALD.OND", "DSM.DRA", "DRA.OND", "DSM.DRD", "DRD.OND")
ncs.sub <- ncs[vars]

## Suicide Attempt Recode (combination)
ncs.sub$attempt <- NA
ncs.sub$attempt[ncs.sub$SD2=="NO" | ncs.sub$SD15=="NO" | ncs.sub$SD5=="NO" | ncs.sub$SD19=="NO"] <- 0
ncs.sub$attempt[ncs.sub$SD5=="YES" | ncs.sub$SD19=="YES"] <- 1

## Date of last suicide attempt (combination)
ncs.sub$last.attempt <- NA
ncs.sub$last.attempt <- ifelse(is.na(ncs.sub$SD10A), ncs.sub$SD23A, ncs.sub$SD10A)

# Re-split on combined/stay-the-same variables, leaving out uncombined suicide variables -- for running through model

# Make variable names usable  

