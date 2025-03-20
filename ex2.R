# Exercise 2 --- dataex2.Rdata

# ------------
load("dataex2.Rdata")
require(mice)
dim(dataex2)

dataex2[,,4][,"X"]

# Parameters
M = 20
true_b1 <- 3
true_b0 = 1


# ----------------------------
# Stochastic Regression Version
# ----------------------------

# set.seed(1)
coverage_stoch <- logical(100)

for(dataset in seq_len(dim(dataex2)[3])){
  df_i <- dataex2[, , dataset]
  imp_s <- mice(df_i, method = "norm", m = M, seed = 1)
  
  # Fit linear model for each imputed dataset
  fit_s <- with(imp_s, lm(Y ~ X))
  
  # Pool using rubin's rule and get CI 95%
  summary_s <- summary(pool(fit_s), conf.int = TRUE)
  lower_stoch <- summary_s["2.5 %"][2,]
  upper_stoch <- summary_s["97.5 %"][2,]
  coverage_stoch[dataset] <- (true_b1 >= lower_stoch & true_b1 <= upper_stoch)
}

empirical_coverage_s <- mean(coverage_stoch)
empirical_coverage_s


# ----------------------------
# Boot strap version
# ----------------------------

# set.seed(1)
coverage_boot <- logical(100)

for(dataset in seq_len(dim(dataex2)[3])){
  df_i <- dataex2[, , dataset]
  imp_b <- mice(df_i, method = "norm.boot", m = M, seed = 1)
  
  # Fit linear model for each imputed dataset
  fit_b <- with(imp_b, lm(Y ~ X))
  
  # Pool using rubin's rule and get CI 95%
  summary_b <- summary(pool(fit_b), conf.int = TRUE)
  lower_boot <- summary_b["2.5 %"][2,]
  upper_boot <- summary_b["97.5 %"][2,]
  coverage_boot[dataset] <- (true_b1 >= lower_boot & true_b1 <= upper_boot)
}

empirical_coverage_b <- mean(coverage_boot)
empirical_coverage_b



