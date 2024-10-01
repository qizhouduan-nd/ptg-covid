## EM 9.29 attempt
library(numDeriv)  
library(LNIRT)
## The number of test takers and number of items 
N = 50
J = 5
## ground truth for person and item parameters
tau = round(rnorm(N, 0, 0.25), 3)
beta = round(rnorm(J, 1.8, 0.25), 3)
alpha = round(runif(J, 1.75, 3.25), 3)

### data generation
generate_data <- function(N, J, true_tau, true_beta, true_alpha, c){
  means <- beta - tau
  time_matrix <- matrix(nrow = N, ncol = J, NA)
  for(j in 1:J){
    item_store <- rep(NA, N)
    for(i in 1:N){
      generated_time <- rnorm(1, mean = means[i], sd = 1/alpha[j])
      item_store[i] <- generated_time
    }
    time_matrix[,j] <- item_store
  }
  censored_data <- ifelse(time_matrix > c, c, time_matrix)
  censor_rate <- sum(time_matrix > c) / (N*J)
  return(list(time_matrix, censored_data, censor_rate))
}

generate_data(N, J, tau, beta, alpha, 2.5)


