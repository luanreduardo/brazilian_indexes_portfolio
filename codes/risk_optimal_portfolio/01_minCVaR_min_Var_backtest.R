##### code 001 #####

####minimum-CVaR vs minimum-variance port: back-test

library(FRAPO) #for the data set
library(fPortfolio) #for the routines

#retriving data and calculating returns
br_indexes <- read.csv('data/pricesIndBr.csv')
rownames(br_indexes) <- br_indexes[, 'Index']
br_indexes <- br_indexes[, -1]
br_returns <- na.omit(timeSeries(returnseries(br_indexes, method = 'discrete'),
                                  charvec = rownames(br_indexes))) #computing discrete % returns

#specifying portfolio
pspec <- fPortfolio::portfolioSpec() #default specification

gmv <- pspec #defining GMV as a portfolio with default specifications

cvar <- pspec #defining a port with default for later ajustments for CVaR
fPortfolio::setType(cvar) <- 'CVaR' #type adjustment of the CVaR portfolio
fPortfolio::setAlpha(cvar) <- .10 #confidence level
fPortfolio::setSolver(cvar) <- "solveRglpk.CVAR" #solver adjustment of the CVaR port

#conducting back-test
end <- time(br_returns)[1260:5875] #end point for sliding window
from <- time(br_returns)[1:length(end)] #start point for sliding window
wGMV <- matrix(NA, ncol = ncol(br_returns), nrow = length(end)) #defining matrices, weights as rows
wCVAR <- wGMV

#TAKES A LONG TIME TO COMPUTE
for (i in 1:length(end)) {
  series <- window(br_returns, start = from[i], end = end[i]) #relevant data window
  gmvpf <- fPortfolio::minvariancePortfolio(data = series, spec = gmv,
                                constraints = 'LongOnly')
  wGMV[i, ] <- c(getWeights(gmvpf))
  cvarpf <- fPortfolio::minriskPortfolio(data = series, spec = cvar,
                             constraints = 'LongOnly')
  wCVAR[i, ] <- c(getWeights(cvarpf))
  print(i)
}
