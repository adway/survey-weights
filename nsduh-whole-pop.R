load("nsduh-pop.Rdata")
library(survey)
library(randomForest)
library(caret)
source("weighted-metrics.R")

tuwauc <- data.frame()
twauc <- data.frame()
tuwsens <- data.frame()
tuwspec <- data.frame()
twsens <- data.frame()
twspec <- data.frame()
popsens <- data.frame()
popspec <- data.frame()
popauc <- data.frame()

age_bin <- case_when(
  combined_pop$Age == "18-25" ~ 1,
  combined_pop$Age == "26-34" ~ 2,
  combined_pop$Age == "35-49" ~ 3,
  combined_pop$Age == "50-64" ~ 4,
  combined_pop$Age == "> 65" ~ 5,
)

combined_pop$age_bin <- age_bin

cat_n <- c(2300, 1500, 1950, 1750, 2500)

for (i in 1:500){
  balanced_samp <- data.frame()
  for (j in 1:5) {
    hold <- sample_n(combined_pop[combined_pop$age_bin == j,], cat_n[j], replace = FALSE)
    balanced_samp <- rbind(balanced_samp, hold)
  }
  
  age_dist <- data.frame(table(combined_pop$age_bin)) # calculates how many of each age there are in population
  prob <- cat_n[balanced_samp$age_bin]/age_dist$Freq[balanced_samp$age_bin]
  balanced_samp$prob <- prob
  balanced_samp$Weight <- 1/prob
  balanced_samp$fpc <- age_dist$Freq[balanced_samp$age_bin]
  balanced_samp_design <- svydesign(data = balanced_samp, weights = ~Weight, ids = ~1, strata = ~age_bin, fpc = ~fpc)
  
  index <- createDataPartition(balanced_samp$IllDrug, p = 0.8, list = FALSE)
  balanced_sample_train <- balanced_samp[index, ]
  balanced_sample_test <- balanced_samp[-index, ]
  balanced_sample_train$Weight <- balanced_sample_train$Weight*1.25
  balanced_sample_train <- balanced_sample_train[,1:19]
  
  # balanced_sample_train <- upSample(x=balanced_sample_train[,2:19],
                                      # y=balanced_sample_train$IllDrug)
  balanced_sample_train <- balanced_sample_train %>% rename(IllDrug = IllDrug) %>% mutate(Weight = 1)
  
  # Random forest
  
  fit_balanced_train <- randomForest(IllDrug ~ .-Weight, data = balanced_sample_train, split = "gini", ntree = 1000) # Random Forest
  test_probs <- predict(fit_balanced_train, balanced_sample_test, type = "prob")
  test_probs <- test_probs[,2]
  
  tauc <- aucs(test_probs = test_probs, ground_truth = balanced_sample_test$IllDrug, weight = balanced_sample_test$Weight)
  tuwauc <- rbind(tuwauc, tauc$uwauc)
  twauc <- rbind(twauc, tauc$wauc)
  
  balanced_sample_test$pred <- ifelse(test_probs >= 0.5, 1, 0)
  tcm <- weighted_cm(balanced_sample_test$pred, balanced_sample_test$IllDrug, balanced_sample_test$Weight)
  
  tuwsens <- rbind(tuwsens, tcm$sens)
  tuwspec <- rbind(tuwspec, tcm$spec)
  twsens <- rbind(twsens, tcm$wsens)
  twspec <- rbind(twspec, tcm$wspec)
  
  combined_pop$Weight <- 1
  
  pop_probs <- predict(fit_balanced_train, combined_pop, type = "prob")
  pop_probs <- pop_probs[,2]
  combined_pop$pred <- ifelse(pop_probs >= 0.5, 1, 0)
  
  pauc <- aucs(test_probs = pop_probs, ground_truth = combined_pop$IllDrug, weight = combined_pop$Weight)
  popauc <- rbind(popauc, pauc$uwauc)
  pcm <- weighted_cm(combined_pop$pred, combined_pop$IllDrug, weight = combined_pop$Weight)
  popspec <- rbind(popspec, pcm$spec)
  popsens <- rbind(popsens, pcm$sens)
  
  print(i)
  
}

res_balanced_forest <- data.frame(tuwsens, tuwspec, tuwauc, twsens, twspec, twauc, popsens, popspec, popauc)
colnames(res_balanced_forest) <- c("UW Sens.", "UW Spec.", "UW AUC", "W Sens.", "W Spec.", "W AUC", "P Sens.", "P Spec.", "P AUC")
write.csv(res_balanced_forest, file = "results/res-balanced-forest-500.csv")