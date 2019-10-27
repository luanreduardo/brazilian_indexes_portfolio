##### code 008 #####

####risk surface plot of multi-asset portfolio, part one

#install.packages("Rdonlp2", repos="http://R-Forge.R-project.org")
library(FRAPO)
library(fPortfolio)

#loading data set
data(MultiAsset)

#creating time series of prices and returns
pr <- timeSeries(MultiAsset, charvec = rownames(MultiAsset))
data <- returns(pr, method = 'discrete',
                percentage = T, trim = T)

#parameters / constant
NAssets <- ncol(pr)
ANames <- colnames(pr)
Sigma <- cov(data)
mu <- colMeans(data)

#risk surface plot
hull <- markowitzHull(data, nFrontierPoints = 50)
grid <- feasibleGrid(hull, trace = F)
divers <- bestDiversification(grid, trace = F)

#standard deviation of  marginl risk contributions
mrc.sd <- function(data, weights) {
  Sigma <- cov(data)
  a <- mrc(weights, Sigma)
  sd(a)
}
surf <- riskSurface(divers, FUN = 'mrc.sd')

#feasible portfolio with highest diversification ratio
allWeights <- attr(divers, 'weights')
idx <- sort(unique(allWeights[, 1]))
dropt <- matrix(0, nrow = length(idx), ncol = 2)
idxRow <- 1:length(idx)

for (j in idx) {
  w <- matrix(allWeights[allWeights[, 1] == j, -c(1, 2)],
              ncol = NAssets)
  divm <- vector()
  length(divm) <- nrow(w)
  
  for (i in 1:nrow(w)) {
    divm[i] <- dr(w[i, ], Sigma)
  }
  
  divmidx <- which.max(divm)
  wopt <- w[divmidx, ]
  dropt[idxRow[j], ] <- c(crossprod(wopt, mu),
                          sqrt(crossprod(wopt, Sigma)
                               %*% wopt))
}