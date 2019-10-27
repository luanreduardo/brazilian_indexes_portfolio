##### code 007 #####
#previous: rolling_window_optim.R

#### portfolio optimization with elliptical uncertainty - FRAPO 10.7

#defining a function for points on the efficient frontier
#for a fully-invested, long-only portfolio
PMV <- function(SRoot, mu, SigTerm, #sqrt of var-cov matrix, expected return, first term of the classical mean-
                optctrl = ctrl(trace = F)) {                      #variance portfolio optimization
  N <- nrow(SRoot) #number of assets
  #second-order cone constrain for portfolio standard deviation risk
  socl <- socc(F = SRoot, g = rep(0, N),
               d = rep(0, N), f = SigTerm)
  #cone constrain to non-negativity requirement
  nnol <- nnoc(G = -diag(N), h = rep(0, N))
  #contraining by fully-invested requirement
  A1 <- matrix(rep(1, N), nrow = 1)
  b1 <- 1.0
  #optimizing
  ans <- cccp(q = -mu, A = A1, b = b1,
              cList = list(nnol, socl),
              optctrl = optctrl)
  getx(ans)
}