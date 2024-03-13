library(tidyverse)
library(broom)
library(caret)
library(survey)
library(DMwR)
library(randomForest)
library(pracma)
library(pROC)
source("weighted-metrics.R")

load("data/NSDUH_2019.RData")
data <- PUF2017_100918

# DATA CLEANING + PREP

## Filter the observations corresponding to youth.
population <- subset(data, data$CATAG2 != 1)

## Filter missing values corresponding to each of the different types
#disabilities and make disability score
population <- subset(population, population$diffsee == 1 | population$diffsee == 2)
population <- subset(population, population$diffhear == 1 | population$diffhear == 2)
population <- subset(population, population$diffthink == 1 | population$diffthink == 2)
population <- subset(population, population$diffwalk == 1 | population$diffwalk == 2)
population <- subset(population, population$diffdress == 1 | population$diffdress == 2)
population <- subset(population, population$differand == 1 | population$differand == 2)

population$disability <- population$diffsee + population$diffhear + population$diffwalk + population$diffthink + population$diffdress + population$differand
population$disability[population$disability != 12] <- 0
population$disability[population$disability == 12] <- 1

## Select the variables to include in the model
final_pop <- data.frame()
final_pop <- data.frame(population$illyr, population$disability, population$irsex, population$NEWRACE2, population$CATAG6, population$income, population$AMIYR_U, population$eduhighcat, population$HEALTH2, population$IRWRKSTAT18, population$FUALC18, population$FUMJ18, population$parol, population$prob, population$grskhertry, population$grskherwk, population$difobther, population$APPDRGMON2, population$BMI2, population$ANALWT_C)
final_pop <- na.omit(final_pop)
colnames(final_pop) <-  c("IllDrug","Disability","Gender","Race","Age","Income","MentalIllness","Edu","Health","Empl","AlcAge","MJAge","Parol","Probation","Lifetime","Weekly","Easy","Approach","Obesity", "Weight")

## Convert numerics/multilevels to factors

final_pop$Obesity[final_pop$Obesity < 25] <- 0
final_pop$Obesity[final_pop$Obesity >= 25 & final_pop$Obesity < 30] <- 1
final_pop$Obesity[final_pop$Obesity >= 30] <- 2

###### final_pop$Opioid[final_pop$Opioid == 1] <- "OUD"
###### final_pop$Opioid[final_pop$Opioid == 0] <- "No-OUD"

final_pop$MJAge[final_pop$MJAge == 1] <- "< 18"
final_pop$MJAge[final_pop$MJAge == 2] <- "> 18/No"

final_pop$MentalIllness[final_pop$MentalIllness == 1] <- "Yes"
final_pop$MentalIllness[final_pop$MentalIllness == 0] <- "No"

final_pop$Income[final_pop$Income == 1] <- "<20k"
final_pop$Income[final_pop$Income == 2] <- "20k-49k"
final_pop$Income[final_pop$Income == 3] <- "50k-74k"
final_pop$Income[final_pop$Income == 4] <- ">75k"

final_pop$Edu[final_pop$Edu == 1] <- "< HS"
final_pop$Edu[final_pop$Edu == 2] <- "HS"
final_pop$Edu[final_pop$Edu == 3] <- "C/A"
final_pop$Edu[final_pop$Edu == 4] <- "C"

final_pop$Health[final_pop$Health == 1] <- "E"
final_pop$Health[final_pop$Health == 2] <- "VG"
final_pop$Health[final_pop$Health == 3] <- "G"
final_pop$Health[final_pop$Health == 4] <- "F/P"

final_pop$AlcAge[final_pop$AlcAge == 1] <- "< 18"
final_pop$AlcAge[final_pop$AlcAge == 2] <- "> 18/No"

final_pop$Disability[final_pop$Disability == 1] <- "Yes"
final_pop$Disability[final_pop$Disability == 2] <- "No"

final_pop$Gender[final_pop$Gender == 1] <- "Male"
final_pop$Gender[final_pop$Gender == 2] <- "Female"

final_pop$Race[final_pop$Race == 1] <- "W"
final_pop$Race[final_pop$Race == 2] <- "B"
final_pop$Race[final_pop$Race == 3] <- "N"
final_pop$Race[final_pop$Race == 4] <- "HI"
final_pop$Race[final_pop$Race == 5] <- "A"
final_pop$Race[final_pop$Race == 6] <- ">1"
final_pop$Race[final_pop$Race == 7] <- "H"

final_pop$Age[final_pop$Age == 2] <- "18-25"
final_pop$Age[final_pop$Age == 3] <- "26-34"
final_pop$Age[final_pop$Age == 4] <- "35-49"
final_pop$Age[final_pop$Age == 5] <- "50-64"
final_pop$Age[final_pop$Age == 6] <- "> 65"

final_pop$Empl[final_pop$Empl == 1] <- "F"
final_pop$Empl[final_pop$Empl == 2] <- "P"
final_pop$Empl[final_pop$Empl == 3] <- "U"
final_pop$Empl[final_pop$Empl == 4] <- "O"

final_pop$Income <- factor(final_pop$Income)
final_pop$MentalIllness <- factor(final_pop$MentalIllness)
final_pop$Parol <- factor(final_pop$Parol)
final_pop$Probation <- factor(final_pop$Probation)
final_pop$Lifetime <- factor(final_pop$Lifetime)
final_pop$Weekly <- factor(final_pop$Weekly)
final_pop$Easy <- factor(final_pop$Easy)
final_pop$Approach <- factor(final_pop$Approach)
final_pop$Obesity <- factor(final_pop$Obesity)
final_pop$IllDrug <- factor(final_pop$IllDrug)

combined_pop <- data.frame()
combined_pop <- rbind(combined_pop, final_pop)

save(final_pop, file = "nsduh-with-weights.RData")

# SRS analysis
load("nsduh-pop.RData")
N <- nrow(combined_pop)
n <- 10000

srs_sample <- sample_n(combined_pop, n, replace = FALSE)
srs_sample <- srs_sample %>%
  mutate(weight = N/n)

index <- createDataPartition(srs_sample$IllDrug, p = 0.8, list = FALSE)
srs_sample_train <- srs_sample[index, ]
srs_sample_test <- srs_sample[-index, ]
srs_sample_train <- srs_sample_train %>% 
  mutate(weight = NULL)

srs_sample_train <- SMOTE(IllDrug ~ ., srs_sample_train, k = 5, perc.over = 40, perc.under = 350)

fit_srs_train <- glm(data = srs_sample_train, formula = IllDrug ~ ., family = binomial) # Logistic regression
summary(fit_srs_train)
fit_srs_train <- randomForest(IllDrug ~ ., data = srs_sample_train, split = "gini", ntree = 1000) # Random Forest
test_probs <- predict(fit_srs_train, srs_sample_test, type = "prob")
srs_sample_test$pred <- ifelse(test_probs >= 0.5, 1, 0) # for logistic regression
srs_sample_test$pred <- ifelse(test_probs[,2] >= 0.5, 1, 0) # for RF
cm_srs_test <- confusionMatrix(factor(srs_sample_test$pred), factor(srs_sample_test$IllDrug), positive = "1")
cm_srs_test

pop_probs <- predict(fit_srs_train, combined_pop, type = "response")
combined_pop$pred <- ifelse(pop_probs >= 0.5, 1, 0) # for LR
combined_pop$pred <- pop_probs # for RF
cm_pop <- confusionMatrix(factor(combined_pop$pred), factor(combined_pop$IllDrug), positive = "1")
cm_pop

##### Stratified design #####
set.seed(111)
age_bin <- case_when(
    combined_pop$Age == "18-25" ~ 1,
    combined_pop$Age == "26-34" ~ 2,
    combined_pop$Age == "35-49" ~ 3,
    combined_pop$Age == "50-64" ~ 4,
    combined_pop$Age == "> 65" ~ 5,
  )

combined_pop$age_bin <- age_bin

cat_n <- c(2300, 1500, 1950, 1750, 2500) # number of people selected from each category
cat_n <- round(c(0.32951927, 0.20514555, 0.26367537, 0.11451598, 0.08714382)*n)
## creates sample of selected numbers of each age bin
balanced_samp <- data.frame()
for (i in 1:5) {
  hold <- sample_n(combined_pop[combined_pop$age_bin == i,], cat_n[i], replace = FALSE)
  balanced_samp <- rbind(balanced_samp, hold)
}
## give each person in the sample a probability of selection and a weight
age_dist <- data.frame(table(combined_pop$age_bin)) # calculates how many of each age there are in population
prob <- cat_n[balanced_samp$age_bin]/age_dist$Freq[balanced_samp$age_bin]
balanced_samp$prob <- prob
balanced_samp$weight <- 1/prob
balanced_samp$fpc <- age_dist$Freq[balanced_samp$age_bin]
balanced_samp_design <- svydesign(data = balanced_samp, weights = ~weight, ids = ~1, strata = ~age_bin, fpc = ~fpc)


########==============HERE ONWARDS FOsR REPS==========###########
index <- createDataPartition(balanced_samp$IllDrug, p = 0.8, list = FALSE)
balanced_sample_train <- balanced_samp[index, ]
balanced_sample_test <- balanced_samp[-index, ]
balanced_sample_train <- balanced_sample_train %>% 
  mutate(weight = NULL)

balanced_sample_train_up <- upSample(x=balanced_sample_train[,2:22],
                                     y=balanced_sample_train$IllDrug)
balanced_sample_train_up <- balanced_sample_train_up %>% rename(IllDrug = Class)

fit_balanced_train <- glm(data = balanced_sample_train, formula = IllDrug ~ . -age_bin -prob -fpc, family = binomial)
#summary(fit_balanced_train)
fit_balanced_train <- randomForest(IllDrug ~ .-age_bin -prob -fpc, data = balanced_sample_train_up, split = "gini", ntree = 1000) # Random Forest
test_probs <- predict(fit_balanced_train, balanced_sample_test, type = "prob")
test_probs <- test_probs[,2]

#auc(factor(balanced_sample_test$IllDrug), test_probs)
aucs(test_probs = test_probs, ground_truth = balanced_sample_test$IllDrug, weight = balanced_sample_test$weight)

balanced_sample_test$pred <- ifelse(test_probs >= 0.5, 1, 0)
#balanced_sample_test$pred <- test_probs

cm_srs_test <- confusionMatrix(factor(balanced_sample_test$pred), factor(balanced_sample_test$IllDrug), positive = "1")
cm_srs_test
weighted_cm(balanced_sample_test$pred, balanced_sample_test$IllDrug, weight = balanced_sample_test$weight)

### CLEAN UPs A BUG WITH PREDICT FUNCTION ###
combined_pop$fpc <- rnorm(N, 1300, 5)
combined_pop$prob <- rnorm(N, 0.5, 0.1)
### ###
pop_probs <- predict(fit_balanced_train, combined_pop, type = "prob")
combined_pop$pred <- ifelse(pop_probs[,2] >= 0.5, 1, 0)
#combined_pop$pred <- pop_probs
cm_pop <- confusionMatrix(factor(combined_pop$pred), factor(combined_pop$IllDrug), positive = "1")
cm_pop
auc(factor(combined_pop$IllDrug), pop_probs[,2])
#comb_pop_auc <- popauc(pop_probs[,2], factor(combined_pop$IllDrug))
#points(1 - comb_pop_auc$X0, comb_pop_auc$X1, col="green", pch = "y")
#legend(x="bottomright", legend=c("Weighted ROC", "Unweighted ROC", "Population ROC"), 
       fill = c("blue","black", "green")
)

weighted_dist <- comb_pop_auc - test_aucs$wpairs
weighted_dist <- weighted_dist*weighted_dist
weighted_dist.1 <- sum(sqrt(weighted_dist$X0)) + sum(sqrt(weighted_dist$X1)) / 101

uweighted_dist <- comb_pop_auc - test_aucs$pairs
uweighted_dist <- uweighted_dist*uweighted_dist
uweighted_dist.1 <- sum(sqrt(uweighted_dist$X0)) + sum(sqrt(uweighted_dist$X1)) / 101


