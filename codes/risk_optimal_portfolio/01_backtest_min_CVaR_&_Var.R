#### Based on cap 12 of Financial Risk Modelling
####and Portfolio Optimization by Berhnard Pfaff

#code 01

##minimum-CVaR vs minimum-variance portfolios: back-test

library(FRAPO) #for the data set
library(fPortfolio) #for the routines

#retriving data and calculating returns
br_indexes <- read.csv('data/pricesIndBr.csv')
rownames(br_indexes) <- br_indexes[, 'Index']
br_indexes <- br_indexes[, -1]
br_returns <- na.omit(returnseries(br_indexes, method = 'discrete'),
                                 charvec = rownames(br_indexes)) #computing discrete % returns

saveRDS(br_indexes, 'data/br_indexes.Rds')
saveRDS(br_returns, 'data/br_returns.Rds')


#defining timeseries
ts_br_indexes <- timeSeries(br_indexes)
ts_br_returns <- timeSeries(br_returns)

#specifying portfolio
pspec <- fPortfolio::portfolioSpec() #default specification

gmv <- pspec #defining GMV as a portfolio with default specifications

cvar <- pspec #defining a port with default for later ajustments for CVaR
fPortfolio::setType(cvar) <- 'CVaR' #type adjustment of the CVaR portfolio
fPortfolio::setAlpha(cvar) <- .10 #confidence level
fPortfolio::setSolver(cvar) <- "solveRglpk.CVAR" #solver adjustment of the CVaR port

#conducting back-test
end <- time(ts_br_returns)[1260:5875] #end point for sliding window
from <- time(ts_br_returns)[1:length(end)] #start point for sliding window
wGMV <- matrix(NA, ncol = ncol(ts_br_returns), nrow = length(end)) #defining matrices, weights as rows
wCVAR <- wGMV

#TAKES TIME TO COMPUTE
for (i in 1:length(end)) {
  series <- window(ts_br_returns, start = from[i], end = end[i]) #relevant data window
  gmvpf <- fPortfolio::minvariancePortfolio(data = series, spec = gmv,
                                            constraints = 'LongOnly')
  wGMV[i, ] <- c(getWeights(gmvpf))
  cvarpf <- fPortfolio::minriskPortfolio(data = series, spec = cvar,
                                         constraints = 'LongOnly')
  wCVAR[i, ] <- c(getWeights(cvarpf))
  print(i)
}

#saving weights
saveRDS(wGMV, file = 'data/GMV_weights.RDS')
saveRDS(wCVAR, file = 'data/CDaR_weights.RDS')

####plotting wealth trajectory

#lagged portfolio weights for equity lines computing
wGMVL1 <- lag(timeSeries(wGMV, charvec = end), k = 1)
colnames(wGMVL1) <- colnames(ts_br_returns)
wCVARL1 <- lag(timeSeries(wCVAR, charvec = end), k = 1)
colnames(wCVARL1) <- colnames(ts_br_returns)

#computing od returns according to the pre-definied weights, lagged a period
GMVRetFac <- 1 + rowSums(wGMVL1 * ts_br_returns[time(wGMVL1), ]) / 100 #return factor
GMVRetFac[1] <- 100
GMVPort <- timeSeries(cumprod(GMVRetFac), 
                      charvec = names(GMVRetFac)) #portfolio values

CVARRetFac <- 1 + rowSums(wCVARL1 * ts_br_returns[time(wCVARL1), ]) / 100 #return factor
CVARRetFac[1] <- 100
CVARPort <- timeSeries(cumprod(CVARRetFac),
                       charvec = names(CVARRetFac)) #portfolio values

saveRDS(GMVPort, file = 'data/GMV_port_values.RDS')
saveRDS(wCVAR, file = 'data/CDaR_port_values.RDS')

#plotting of portfolio values
ylims <- range(cbind(GMVPort, CVARPort))
plot(GMVPort, ylim = ylims, xlab = '',
     ylab = 'Portfolio Value (Index)')
lines(CVARPort, col = 'blue')
legend('topleft',
       legend = c('Minimum-Variance', 'Minimum-CVaR'),
       col = c('black', 'blue'), lty = 1)

dev.copy(png, 'results/port_value.png') #saving the graph
dev.off()

#relative performance
RelOutPerf <- (CVARPort - GMVPort) / GMVPort * 100
plot(RelOutPerf, type = 'h', col = 'blue', xlab = '',
     ylab = 'Percentage',
     main = 'Relative out-performance Min-CVaR vs. Min-Variance')
abline(h = 0, col = 'grey')

dev.copy(png, 'results/relative_performance.png')
dev.off()
