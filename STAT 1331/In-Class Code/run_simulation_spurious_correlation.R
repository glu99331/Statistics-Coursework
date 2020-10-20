rm(list = ls()) # clear the data environment
setwd("C:/Users/gordo/Desktop/Fall 2020 Classes/STAT 1331 - Financial Econometrics/In-Class Code") # set the working directory

source("generate_plus_test.R") # add functions inside the local R file named generate_plus_test

TT <- 300 # number of observations
#if lambda and rho both != 0, we have reject a lot more, when we shouldn't have!
lam <- 0.9 # determines the degree of serial correlation for y
rho <- 0.9 # determines the degree of serial correlation for x
alpha <- 0.05 # size of the t test
nsim <- 1000 # number of simulations
reject_null <- matrix(FALSE,nrow = nsim, ncol = 1)
for (i in 1:nsim) {
  set.seed(124*i) # set the seed for generating data
  reject_null[i] <- gen_test(TT, lam, rho, alpha)
}
estimated_size <- sum(reject_null)/nsim

print(estimated_size)

