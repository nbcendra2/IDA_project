# Exercise 5 --- dataex5.Rdata

# ------------
load("dataex5.Rdata")
obs_idx <- which(!is.na(dataex5$Y))
miss_idx <- which (is.na(dataex5$Y))
n <- length(dataex5$Y) # total data
m <- length(obs_idx) # num of observed data

# Q Function ---- E step -----
q_func <- function(par, X, Yobs, Yhat, idx_obs, idx_miss){
  beta0 <- par[1]
  beta1 <- par[2]
  
  # defining pi(beta)
  linpred <- beta0 + beta1 * X
  p <- exp(linpred)/ (1 + exp(linpred))
  p_obs <- p[idx_obs]
  p_miss <- p[idx_miss]
  
  # obs data i to m
  log_lik_obs <- sum(Yobs * log(p_obs) + (1-Yobs) * log(1-p_obs))
  # miss data m+1 to n
  log_lik_miss <- sum(Yhat * log(p_miss) + (1-Yhat) * log (1-p_miss))
  
  return(-(log_lik_obs + log_lik_miss)) # return negative log like
}

# EM algorithm function
EM_run <- function(x, y, start = c(0, 0), eps = 1e-6 ) {
  iobs  <- which(!is.na(y))
  imiss <- which(is.na(y))
  
  # Current parameter guess
  beta_curr <- start
  Yobs <- y[iobs]
  
  diff <- Inf
  
  while (diff > eps) {
    
    linpred <- beta_curr[1] + beta_curr[2]*x
    p       <- exp(linpred)/ (1 + exp(linpred))
    Yhat    <- p[imiss]
    
    # ---- E Step ----
    # ---- M Step ----
    res <- optim(
      par    = beta_curr,
      fn     = q_func,       
      X      = x,
      Yobs   = Yobs,
      Yhat   = Yhat,
      idx_obs   = iobs,
      idx_miss  = imiss,
      method = "BFGS"
    )
    
    beta_new <- res$par
    diff     <- sum(abs(beta_new-beta_curr))
    beta_curr <- beta_new
  }
  
  list(beta = beta_curr, value = res$value)
}


# Run EM
fit_em <- EM_run(dataex5$X, dataex5$Y)
fit_em$beta # beta

