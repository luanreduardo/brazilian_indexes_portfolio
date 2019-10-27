##### code 009 #####
#previous: classical_and_robust_optimization.R

#### equivalent classical allocation for a given robust risk weighting - FRAPO 10.9

#computes the equivalent risk weighting for mean-variance portfolio
#args: theta (given risk weighting of robust conterparts)
# level (confidence level for computing quantile by the /chi squared dist.)
theta2lambda <- function(theta, Nassets, Nobs, level) {
  delta <- sqrt(qchisq(level, Nassets))
  lambda <- theta / (1 + theta * (delta / sqrt(Nobs)))
  return(lambda)
}

#robust and equivalent classical allocation
theta <- .7 #defined
#robust allocation for the theta defined above
wrc <- PMV(SRoot = SR, mu = mu, SigTerm = SigMin / theta) 
#points on the efficient frontier (robust)
rceq <- c(sqrt(t(wrc) %*% S %*% wrc), crossprod(mu, wrc))

#using the defined function to get the equivalent risk weighting
rweq <- theta2lambda(theta, Nassets = Nassets, Nobs = Nobs, level = .9)
#equivalent classical mean-variance allocation
wmv <- PMV(SRoot = SR, mu = mu, SigTerm = SigMin / rweq)
#points on the efficient frontier (mean-variance)
mveq <- c(sqrt(t(wmv) %*% S %*% wmv), crossprod(mu, wmv))
