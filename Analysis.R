library(Hmisc)
library(tidyr)
library(ranger)
library(pROC)
library(dplyr)
library(readr)

ncs <- spss.get("./data/DS0002/20240-0002-Data.sav", use.value.labels=TRUE)

vars <- c("SD2", "SD15", "SD6", "SD10A", "SD19", "SD23A", "DSM.PDS", "PD.OND", "DSM.AGO", "AGO.OND", "DSM.SP", "SP.OND", "DSM.SO", "SO.OND", "DSM.PTS", "PTS.OND", "DSM.GAD", "GAD.OND", "DSM.MDDH", "MDDH.OND", "DSM.ALA", "ALA.OND", "DSM.ALD", "ALD.OND", "DSM.DRA", "DRA.OND", "DSM.DRD", "DRD.OND")
ncs.sub <- ncs[vars]

# Suicide Attempt Recode
ncs.sub$attempt <- NA
ncs.sub$attempt[ncs.sub$SD2=="NO" | ncs.sub$SD15=="NO" | ncs.sub$SD5=="NO" | ncs.sub$SD19=="NO"] <- 0
ncs.sub$attempt[ncs.sub$SD5=="YES" | ncs.sub$SD19=="YES"] <- 1
