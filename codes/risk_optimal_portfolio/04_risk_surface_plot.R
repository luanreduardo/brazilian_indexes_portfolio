#### Based on cap 12 of Financial Risk Modelling
####and Portfolio Optimization by Berhnard Pfaff

#code 04

##risk surface plot of multi-asset portfolio

#install.packages("Rdonlp2", repos = "http://R-Forge.R-project.org")

library(FRAPO)
library(fPortfolio)
library(Rdonlp2)

br_indexes <- readRDS('data/br_indexes.Rds')

#creating time series of prices and returns
pr <- timeSeries(br_indexes, charvec = rownames(br_indexes))
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
  print(j)
}

###surface plot with superimposed mdp solutions

#per standard deviation risk level
surfacePlot(surf, type = 'filled.contour',
            palette = topo.colors, addHull = T,
            addGrid = F, addAssets = F,
            xlab = 'Target Risk', ylab = 'Target Return',
            main = 'Convex Hull with Risk Surface:\nStd.Dev. of MRC and MDP-line')
#greatest diversification ratio for each risk level
lines(x = dropt[, 2], y = dropt[, 1], col = 'red', lwd = 2)
box()

#computing special points and plotting
frontier <- portfolioFrontier(data)
MVP <- minvariancePoints(frontier)
TGP <- tangencyPoints(frontier)
sap <- singleAssetPoints(frontier)

wewp <- rep(1/NAssets, NAssets)
mewp <- crossprod(mu, wewp)
sewp <- sqrt(crossprod(wewp, Sigma) %*% wewp)
ERC <- PERC(Sigma)

werc <- Weights(ERC) / 100.0
merc <- crossprod(mu, werc)
serc <- sqrt(crossprod(werc, Sigma) %*% werc)

points(sap, col = 'darkgreen', pch = 19, cex = .8)
text(sap, ANames, col = 'darkred', cex = .6, pos = 4)
points(TGP, col = 'tan', pch = 19, cex = 2.5)
text(TGP[1],  TGP[2], 'TGP', col = 'purple', cex = .5)
points(x = sewp, y = mewp, col = 'tan', pch = 19, cex = 2.5)
text(sewp, mewp, 'EWP', col = 'purple', cex = .5)
points(x = serc, y = merc, col = 'tan', pch = 19, cex = 2.5)
text(serc, merc, 'ERC', col = 'purple', cex = .5)
points(MVP, col = 'tan', pch = 19, cex = 2.5)
text(MVP[1], MVP[2], 'MVP', col = 'purple', cex = .5)


sdmrc <- surf$z
#choosing feasible portfolios around the contour line 21.9
c <- which((sdmrc >= 21.80) & (sdmrc <= 22),
              arr.ind = T)
w <- matrix(NA, nrow = nrow(c), ncol = NAssets)
colnames(w) <- ANames

for (i in 1:nrow(c219)) {
  gidx <- which((allWeights[, 1] == c[i, 1]) &
                  (allWeights[, 2] == c[i, 2]),
                arr.ind = T)
  w[i, ] <- allWeights[gidx, -c(1, 2)]
}

#computing standard deviation of mrc and standard deviation risk
sdmrc <- apply(w, 1, function(x) sd(mrc(x, Sigma = Sigma)))
sdr <- apply(w, 1, function(x) sqrt(crossprod(x, Sigma) %*% x)) * 100

#grouping by asset class
wDollar <- w[, 1:2]
wIndex <- w[, 3:6]
wGold <- w[, 7]
wTBF <- w[, 8]
wDollar <- rowSums(wDollar)
wIndex <- rowSums(wIndex)
wAsset <- cbind(wDollar, wIndex, wGold, wTBF) * 100
ans <- cbind(wAsset, sdmrc, sdr)
colnames(ans) <- c('Dollar', 'Index', 'Gold', 'TBF', 'StdDev. of MRC', 'StdDev. Risk')
rownames(ans) <- 1:nrow(ans)