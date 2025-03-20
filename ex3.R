# Exercise 3 --- dataex3.Rdata

# ------------
load("dataex3.Rdata")


dataex3

str(dataex3)

dataex3

# Define parameter
sigma <- 1.5 
D <- min(dataex3$X[dataex3$R == 0]) # "missing" data

# Define negative log likelihood
neg_log_like <- function(X, R, mu, sigma, D){
  
  log_phi <- dnorm(X, mean = mu, sd = sigma, log = TRUE)
  
  log_Phi <- pnorm (D, mean = mu, sd = sigma, log.p = TRUE)
  
  neg_log <- -sum(R* log_phi + (1-R) * log_Phi)
  
  return (neg_log) # return negative log likelihood to be optimize with optim()
}

# Run optimization ---- get MLE
init_mu <- mean(dataex3$X)

mle <- optim(par = init_mu, fn = neg_log_like,
             X = dataex3$X, R = dataex3$R, 
             sigma = sigma, D = D,
             method = "BFGS")

# MLE Result
mle_mu <- mle $par
mle_mu

