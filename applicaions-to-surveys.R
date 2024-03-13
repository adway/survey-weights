load("nsduh-with-weights.RData")
load("ncsr-with-weights.RData")
load("nsduh-pop.Rdata")
library(survey)
library(randomForest)
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

# for (i in 1:500) {
  
  age_bin <- case_when(
    combined_pop$Age == "18-25" ~ 1,
    combined_pop$Age == "26-34" ~ 2,
    combined_pop$Age == "35-49" ~ 3,
    combined_pop$Age == "50-64" ~ 4,
    combined_pop$Age == "> 65" ~ 5,
  )
  
  combined_pop$age_bin <- age_bin
  
  cat_n <- c(2300, 1500, 1950, 1750, 2500) # number of people selected from each category
  # cat_n <- round(c(0.32951927, 0.20514555, 0.26367537, 0.11451598, 0.08714382)*n)
  ## creates sample of selected numbers of each age bin
  balanced_samp <- data.frame()
  for (j in 1:5) {
    hold <- sample_n(combined_pop[combined_pop$age_bin == j,], cat_n[j], replace = FALSE)
    balanced_samp <- rbind(balanced_samp, hold)
  }
  ## give each person in the sample a probability of selection and a weight
  age_dist <- data.frame(table(combined_pop$age_bin)) # calculates how many of each age there are in population
  prob <- cat_n[balanced_samp$age_bin]/age_dist$Freq[balanced_samp$age_bin]
  balanced_samp$prob <- prob
  balanced_samp$weight <- 1/prob
  balanced_samp$fpc <- age_dist$Freq[balanced_samp$age_bin]
  balanced_samp_design <- svydesign(data = balanced_samp, weights = ~weight, ids = ~1, strata = ~age_bin, fpc = ~fpc)


index <- createDataPartition(final_pop$attempt, p = 0.8, list = FALSE)
balanced_sample_train <- final_pop[index, ]
balanced_sample_test <- final_pop[-index, ]
balanced_sample_train$Weight <- balanced_sample_train$Weight*1.25
# balanced_sample_train_design <- svydesign(data = balanced_sample_train, weights = ~Weight, ids = ~1, strata = ~age_bin, fpc = ~fpc)

balanced_sample_train <- downSample(x=balanced_sample_train[,1:11],
                                    y=balanced_sample_train$attempt)
balanced_sample_train <- balanced_sample_train %>% rename(attempt = Class) %>% mutate(Weight = 1)

fit_balanced_train <- randomForest(attempt ~ .-Weight, data = balanced_sample_train, split = "gini", ntree = 1000) # Random Forest
# fit_balanced_train <- glm(data = balanced_sample_train, formula = IllDrug ~ .-age_bin-prob-fpc, family = binomial)
# fit_balanced_train <- svyglm(design = balanced_sample_train_design, formula = IllDrug ~ .-age_bin-prob-fpc, family = binomial)
test_probs <- predict(fit_balanced_train, balanced_sample_test, type = "prob")
test_probs <- test_probs[,2]

# tauc <- aucs(test_probs = test_probs, ground_truth = balanced_sample_test$attempt, weight = balanced_sample_test$Weight)
# tuwauc <- rbind(tuwauc, tauc$uwauc)
# twauc <- rbind(twauc, tauc$wauc)

balanced_sample_test$pred <- ifelse(test_probs >= 0.5, 1, 0)
balanced_sample_test$POS <- ifelse(balanced_sample_test$attempt == 1, 1, 0)
balanced_sample_test$NEG <- ifelse(balanced_sample_test$attempt == 0, 1, 0)
tcm <- weighted_cm(balanced_sample_test$pred, balanced_sample_test$attempt, balanced_sample_test$Weight)
design <- svydesign(id = ~1, weights = ~balanced_sample_test$Weight, data = balanced_sample_test)

sens <- svyratio(numerator = tcm$wtpv, denominator = balanced_sample_test$POS, design = design)
spec <- svyratio(numerator = tcm$wtnv, denominator = balanced_sample_test$NEG, design = design)

tuwsens <- rbind(tuwsens, tcm$sens)
tuwspec <- rbind(tuwspec, tcm$spec)
twsens <- rbind(twsens, sens)
twspec <- rbind(twspec, spec)

# combined_pop$fpc <- rnorm(N, 1300, 5)
# combined_pop$prob <- rnorm(N, 0.5, 0.1)
# pop_probs <- predict(fit_balanced_train, combined_pop, type = "response")
# combined_pop$pred <- ifelse(pop_probs >= 0.5, 1, 0)

# pauc <- aucs(test_probs = pop_probs, ground_truth = combined_pop$IllDrug, weight = rep(1, N))
# popauc <- rbind(popauc, pauc$uwauc)

# pcm <- weighted_cm(combined_pop$pred, combined_pop$IllDrug, weight = rep(1, N))
# popspec <- rbind(popspec, pcm$spec)
# popsens <- rbind(popsens, pcm$sens)

print(i)


}

tuwsens <- as.numeric(unlist(tuwsens))
tuwspec <- as.numeric(unlist(tuwspec))
tuwauc <- as.numeric(unlist(tuwauc))

twsens <- as.numeric(unlist(twsens))
twspec <- as.numeric(unlist(twspec))
twauc <- as.numeric(unlist(twauc))

popsens <- as.numeric(unlist(popsens))
popspec <- as.numeric(unlist(popspec))
popauc <- as.numeric(unlist(popauc))

metrics <- data.frame(tuwsens, tuwspec, tuwauc, twsens, twspec, twauc)
metrics <- data.frame(tuwsens, tuwspec, twsens, twspec, popsens, popspec)
write.csv(metrics, "complex-design-ncs-data-BALANCED.csv")

ncsr_vars <- data.frame(twsens, twspec)
colnames(ncsr_vars) <- c("Sensitivity", "Sens.Var", "Specificity", "Spec.Var")
write.csv(ncsr_vars, "ncsr_vars.csv")

colMeans(metrics)

metrics <- read.csv(file = "results/complex-design-ncs-data-BALANCED.csv")

1.676551*sd(metrics$twauc)/sqrt(50)

