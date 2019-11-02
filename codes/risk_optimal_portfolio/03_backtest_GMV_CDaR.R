#code 03

####back-test GMV vs CDaR portfolio optimization


library(FRAPO)
library(fPortfolio)
library(PerformanceAnalytics)

br_indexes <- readRDS('data/br_indexes.Rds')

#creating timeseries of prices and returns
pr <- timeSeries(br_indexes, charvec = rownames(br_indexes))
NAssets <- ncol(pr)
RDP <- na.omit((pr / lag(pr, k = 1) - 1) * 100) #discrete returns

###back-test of GMV vs. CDaR, recursive window

#start and end dates
to <- time(RDP)[1260:nrow(RDP)]
from <- rep(start(RDP), length(to))

##portfolio specification
#CDaR portfolio
DDbound <- .10
DDalpha <- .95

#GMV portfolio
mvspec <- portfolioSpec()
BoxC <- c('minsumW[1:NAssets] = 0.0', 'maxsumW[1:NAssets] = 1.0')

#initialising weights matrices
wMV <- wCD <- matrix(NA, ncol = ncol(RDP), nrow = length(to))

#coducting backtest (ENJOY YOUR LIFE, IT'S GONNA TAKE FOREVER)
for (i in 1:length(to)) {
  series <- window(RDP, start = from[i], end = to[i])
  #prices <- window(pr, start = from[i], end = to[i])
  mv <- minvariancePortfolio(data = series,
                             spec = mvspec,
                             constraints = BoxC)
  #cd <- PCDaR(prices, alpha = DDalpha, bound = DDbound, softBudget = T)
  wMV[i, ] <- c(getWeights(mv))
  #wCD[i, ] <- Weights(cd)
  print(i)
}

saveRDS(wMV, 'data/MV_weights.Rds')
#saveRDS(wCD, 'data/CDaR_weights.Rds')

#lagging optimal weights and sub-sample of returns
wMV <- rbind(rep(NA, ncol(RDP)), wMV[-nrow(wMV), ])
wMVL1 <- timeSeries(wMV, charvec = to)
colnames(wMVL1) <- colnames(RDP)

wCD <- rbind(rep(NA, ncol(RDP)), wCD[-nrow(wCD), ])
wCDL1 <- timeSeries(wCD, charvec = to)
colnames(wCDL1) <- colnames(RDP)

RDPback <- RDP[to, ]
colnames(RDPback) <- colnames(RDP)

####backtest evaluation, part 01

#portfolio equities of strategies
MVRetFac <- 1 + rowSums(wMVL1 * RDPback) / 100
MVRetFac[1] <- 100
MVPort <- timeSeries(cumprod(MVRetFac),
                     charvec = names(MVRetFac))

CDRetFac <- 1 + rowSums(wCDL1 * RDPback) / 100
CDRetFac[1] <- 100
CDPort <- timeSeries(cumprod(CDRetFac),
                     charvec = names(CDRetFac))

#progression of wealth
ylims <- range(cbind(MVPort, CDPort))
plot(CDPort, main = '', ylim = ylims, ylab = 'Index values', xlab = '')
lines(MVPort, col = 'darkgrey')
legend('topleft', legend = c('CDaR', 'GMV'),
       col = c('black', 'darkgrey'),
       lty = 1)

#portfolio returns
MVRet <- returns(MVPort, method = 'discrete',
                 percentage = F, trim = T)
CDRet <- returns(CDPort, method = 'discrete',
                 percentage = F, trim = T)

#draw-down table
table.Drawdowns(MVRet)
table.Drawdowns(CDRet)

#plor of draw-down curves
MVD <- 100 * PerformanceAnalytics:::Drawdowns(MVRet)
CDD <- 100 * PerformanceAnalytics:::Drawdowns(CDRet)
plot(CDD, main = '', ylab = 'Percentages', xlab = '',
     ylim = c(min(c(MVD, CDD)), 0))
lines(MVD, col = 'blue')
abline(h = 0, col = 'lightgrey')
abline(h = -10, col = 'lightgrey', lty = 2)
legend('bottomleft', legend = c('CDaR', 'GMV'),
       col = c('black', 'blue'), lty = 1)

####backtest evaluation, part 01

#portfolio equities of strategies
MVRetFac <- 1 + rowSums(wMVL1 * RDPback) / 100
MVRetFac[1] <- 100
MVPort <- timeSeries(cumprod(MVRetFac),
                     charvec = names(MVRetFac))

CDRetFac <- 1 + rowSums(wCDL1 * RDPback) / 100
CDRetFac[1] <- 100
CDPort <- timeSeries(cumprod(CDRetFac),
                     charvec = names(CDRetFac))

#progression of wealth
ylims <- range(cbind(MVPort, CDPort))
plot(CDPort, main = '', ylim = ylims, ylab = 'Index values', xlab = '')
lines(MVPort, col = 'darkgrey')
legend('topleft', legend = c('CDaR', 'GMV'),
       col = c('black', 'darkgrey'),
       lty = 1)

#portfolio returns
MVRet <- returns(MVPort, method = 'discrete',
                 percentage = F, trim = T)
CDRet <- returns(CDPort, method = 'discrete',
                 percentage = F, trim = T)

#draw-down table
table.Drawdowns(MVRet)
table.Drawdowns(CDRet)

#plor of draw-down curves
MVD <- 100 * PerformanceAnalytics:::Drawdowns(MVRet)
CDD <- 100 * PerformanceAnalytics:::Drawdowns(CDRet)
plot(CDD, main = '', ylab = 'Percentages', xlab = '',
     ylim = c(min(c(MVD, CDD)), 0))
lines(MVD, col = 'blue')
abline(h = 0, col = 'lightgrey')
abline(h = -10, col = 'lightgrey', lty = 2)
legend('bottomleft', legend = c('CDaR', 'GMV'),
       col = c('black', 'blue'), lty = 1)