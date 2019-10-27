##### code 001 #####

####minimum-CVaR vs minimum-variance port: back-test

library(FRAPO) #for the data set
library(fPortfolio) #for the routines

#retriving data and calculating returns
data("StockIndex")
StockReturn <- na.omit(timeSeries(returnseries(StockIndex, method = 'discrete'),
                                  charvec = rownames(StockIndex))) #computing discrete % returns

#specifying portfolio
pspec <- fPortfolio::portfolioSpec() #default specification

gmv <- pspec #defining GMV as a portfolio with default specifications

cvar <- pspec #defining a port with default for later ajustments for CVaR
fPortfolio::setType(cvar) <- 'CVaR' #type adjustment of the CVaR portfolio
fPortfolio::setAlpha(cvar) <- .10 #confidence level
fPortfolio::setSolver(cvar) <- "solveRglpk.CVAR" #solver adjustment of the CVaR port

#conducting back-test, subsamples of 60 monthly observations (5yrs)
end <- time(StockReturn)[60:239] #end point for sliding window
from <- time(StockReturn)[1:length(end)] #start point for sliding window
wGMV <- matrix(NA, ncol = ncol(StockReturn), nrow = length(end)) #defining matrices, weights as rows
wCVAR <- wGMV

for (i in 1:length(end)) {
  series <- window(StockReturn, start = from[i], end = end[i]) #relevant data window
  gmvpf <- fPortfolio::minvariancePortfolio(data = series, spec = gmv,
                                constraints = 'LongOnly')
  wGMV[i, ] <- c(getWeights(gmvpf))
  cvarpf <- fPortfolio::minriskPortfolio(data = series, spec = cvar,
                             constraints = 'LongOnly')
  wCVAR[i, ] <- c(getWeights(cvarpf))
}