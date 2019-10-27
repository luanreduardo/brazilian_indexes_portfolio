##### code 008 #####
#previous: opt_elliptical_uncertainty_fun.R

#### efficient frontiers for mean-variance and robust optimization - FRAPO 10.8
#considering ellipcal uncertainty of $/mu$

library(cccp) #package for dealing cone-constrained programs 

#setting parameters
Nassets <- ncol(rzoo) #number of assets
Nobs <- nrow(rzoo) #number of observations
mu <- colMeans(rzoo) #expected return vector
S <- cov(rzoo) #var-cov matrix of the return
SR <- sqrm(S) #square root of var-cov matrix
delta <- sqrt(qchisq(.9, Nassets)) # x's quantile for the returns uncertainty

#determining feasible risk aversion
SigMax <- max(colSds(rzoo))
SigMin <- min(colSds(rzoo))
ra <- seq(SigMin * 1.1, SigMax * .9, length.out = 10) / SigMax
#range encompasses port. sd risks greater than 110% for the least risky and 90% of the riskiest assets

#initializing objects for MV and robust counterpart results
RCans <- MVans <- matrix(NA, nrow = 10, ncol = Nassets + 2)
#RCans (robust counterpart portfolio) e MVans (mean-variance portfolio)

#computing ten points of efficient frontier and allocations
for (i in 1:10) {
  
  #minimum variance
  wmv <- PMV(SRoot = SR, mu = mu, SigTerm = SigMin / ra[i]) #implied allocation by mean-var approach
  MVans[i, ] <- c(sqrt(t(wmv) %*% S %*% wmv), #associate portfolio sd, expected return and allocation itself
                  crossprod(mu, wmv), wmv)
  
  #robust counterpart
  theta <- ra[i] + (1 - ra[i]) * delta / sqrt(Nobs)
  wrc <- PMV(SRoot = SR, mu = mu, SigTerm = SigMin / theta)
  RCans[i, ] <- c(sqrt(t(wrc) %*% S %*% wrc),
                  crossprod(mu, wrc), wrc)
}

#creates two matrices MVans and RCans filled with mean-var and robust optimizations
#the two differ only with respect to the argument SigTerm