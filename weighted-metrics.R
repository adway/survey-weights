library(survey)
library(pracma)
weighted_cm <- function(prediction, ground_truth, weight) {
  wtpv <- ifelse(ground_truth == 1 & prediction == 1, weight, 0)
  wtnv <- ifelse(ground_truth == 0 & prediction == 0, weight, 0)
  wfpv <- ifelse(ground_truth == 0 & prediction == 1, weight, 0)
  wfnv <- ifelse(ground_truth == 1 & prediction == 0, weight, 0)
  
  wtp <- sum(wtpv)
  wtn <- sum(wtnv)
  wfp <- sum(wfpv)
  wfn <- sum(wfnv)
  
  ref_neg <- c(wtn, wfp)
  ref_pos <- c(wfn, wtp)
  
  cm <- data.frame(ref_neg, ref_pos)
  rownames(cm) <- c("0", "1")
  colnames(cm) <- c("0", "1")
  wsens <- wtp / (wtp + wfn)
  wspec <- wtn / (wtn + wfp)
  print(cm)
  print(paste("Weighted sensitivity:", wsens))
  print(paste("Weighted specificity:", wspec))
}

aucs <- function(test_probs, ground_truth, weight){
  steps <- seq(0, 1, 0.01)
  ### Weighted AUC
  sensitivities <- data.frame()
  specificities <- data.frame()
  for(i in 1:length(steps)) {
    prediction <- ifelse(test_probs >= steps[i], 1, 0)
    wtpv <- ifelse(ground_truth == 1 & prediction == 1, weight, 0)
    wtnv <- ifelse(ground_truth == 0 & prediction == 0, weight, 0)
    wfpv <- ifelse(ground_truth == 0 & prediction == 1, weight, 0)
    wfnv <- ifelse(ground_truth == 1 & prediction == 0, weight, 0)
    
    wtp <- sum(wtpv)
    wtn <- sum(wtnv)
    wfp <- sum(wfpv)
    wfn <- sum(wfnv)
    
    wsens <- wtp / (wtp + wfn)
    wspec <- wtn / (wtn + wfp)
    
    sensitivities <- rbind(sensitivities, wsens)
    specificities <- rbind(specificities, wspec)
  }
  wpairs <- data.frame((sensitivities), (specificities))
  ### Unweighted AUC
  sensitivities <- data.frame()
  specificities <- data.frame()
  for(i in 1:length(steps)) {
    prediction <- ifelse(test_probs >= steps[i], 1, 0)
    wtpv <- ifelse(ground_truth == 1 & prediction == 1, 1, 0)
    wtnv <- ifelse(ground_truth == 0 & prediction == 0, 1, 0)
    wfpv <- ifelse(ground_truth == 0 & prediction == 1, 1, 0)
    wfnv <- ifelse(ground_truth == 1 & prediction == 0, 1, 0)
    
    wtp <- sum(wtpv)
    wtn <- sum(wtnv)
    wfp <- sum(wfpv)
    wfn <- sum(wfnv)
    
    wsens <- wtp / (wtp + wfn)
    wspec <- wtn / (wtn + wfp)
    
    sensitivities <- rbind(sensitivities, wsens)
    specificities <- rbind(specificities, wspec)
  }
  pairs <- data.frame(sensitivities, specificities)
  
  ### Plot
  plot(1 - wpairs$X0, wpairs$X1, col = "blue", pch = "x", main = "Weighted and Unweighted ROC Curves", xlab = "1 - specificity", 
       ylab = "sensitivity")
  points(1 - pairs$X0, pairs$X1, col="black", pch = "o")
  
  ## Return approximations of estimates
  print(paste("Unweighted AUC", abs(trapz(1-pairs$X0, pairs$X1))))
  print(paste("Weighted AUC", abs(trapz(1-wpairs$X0, wpairs$X1))))
  
}

popauc <- function(test_probs, ground_truth) {
  ### Unweighted AUC
  steps <- seq(0, 1, 0.01)
  sensitivities <- data.frame()
  specificities <- data.frame()
  for(i in 1:length(steps)) {
    prediction <- ifelse(test_probs >= steps[i], 1, 0)
    wtpv <- ifelse(ground_truth == 1 & prediction == 1, 1, 0)
    wtnv <- ifelse(ground_truth == 0 & prediction == 0, 1, 0)
    wfpv <- ifelse(ground_truth == 0 & prediction == 1, 1, 0)
    wfnv <- ifelse(ground_truth == 1 & prediction == 0, 1, 0)
    
    wtp <- sum(wtpv)
    wtn <- sum(wtnv)
    wfp <- sum(wfpv)
    wfn <- sum(wfnv)
    
    wsens <- wtp / (wtp + wfn)
    wspec <- wtn / (wtn + wfp)
    
    sensitivities <- rbind(sensitivities, wsens)
    specificities <- rbind(specificities, wspec)
  }
  pairs <- data.frame(sensitivities, specificities)
  
  plot(1 - pairs$X0, pairs$X1, col = "blue", pch = "x", main = "Population ROC Curve", xlab = "1 - specificity", 
       ylab = "sensitivity")
  
  print(paste("Population AUC", abs(trapz(1-pairs$X0, pairs$X1))))
}
