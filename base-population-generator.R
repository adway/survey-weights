# Simulate a population from which we can take stratified samples.
library(tidyverse)

set.seed(0331)

population <- data.frame(matrix(nrow = 100000))

# GIVE EVERYONE AN AGE

age_dist <- data.frame(min = c(19, 25, 34, 54, 64), max = c(25, 34, 54, 64, 100), prop = c(0.11387434555, 0.16361256544, 0.3337696335, 0.17277486911, 0.21596858638)) # specifies the bins and their relative proportions
age_bins <- sample(nrow(age_dist), 100000, replace=TRUE, prob = age_dist$prop) # specifies which bin to pull from
age <- round(age_dist$min[age_bins] + runif(1000)*(age_dist$max[age_bins]-age_dist$min[age_bins])) # after bin of age is specified, this creates an age within that bin according to a uniform distribution.
age

population$age <- age

# GIVE EVERYONE A GENDER

gender <- rbinom(100000, 1, 0.5)

population$gender <- gender

# GIVE EVERYONE SMOKER STATUS (based on their age)

smoker_probs <- case_when(
  age_bins == 1 ~ 0.074,
  age_bins == 2 ~ 0.141,
  age_bins == 3 ~ 0.149,
  age_bins == 4 ~ 0.149,
  age_bins == 5 ~ 0.090
)

smoker <- rbinom(100000, 1, prob = smoker_probs) # yields about 12.6% of the "population" smoking, which is consistent with the US population.

population$smoker <- smoker

# GIVE EVERYONE CAVITY STATUS

cavities_prob <- exp(0.321*population$smoker + 0.02*population$age - 0.105*population$gender)/(1 + exp(0.321*population$smoker + 0.02*population$age - 0.105*population$gender))
cavities <- rbinom(100000, 1, prob = cavities_prob)
population$cavities <- cavities

