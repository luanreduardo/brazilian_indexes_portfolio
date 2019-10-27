##### code 002 #####

####plotting wealth trajectory

#lagged portfolio weights for equity lines computing
wGMVL1 <- lag(timeSeries(wGMV, charvec = end), k = 1)
colnames(wGMVL1) <- colnames(StockReturn)
wCVARL1 <- lag(timeSeries(wCVAR, charvec = end), k = 1)
colnames(wCVARL1) <- colnames(StockReturn)

#computing od returns according to the pre-definied weights, lagged a period
GMVRetFac <- 1 + rowSums(wGMVL1 * StockReturn[time(wGMVL1), ]) / 100 #return factor
GMVRetFac[1] <- 100
GMVPort <- timeSeries(cumprod(GMVRetFac), 
                      charvec = names(GMVRetFac)) #portfolio values

CVARRetFac <- 1 + rowSums(wCVARL1 * StockReturn[time(wCVARL1), ]) / 100 #return factor
CVARRetFac[1] <- 100
CVARPort <- timeSeries(cumprod(CVARRetFac),
                       charvec = names(CVARRetFac)) #portfolio values

#plotting of portfolio values
ylims <- range(cbind(GMVPort, CVARPort))
plot(GMVPort, ylim = ylims, xlab = '',
     ylab = 'Portfolio Value (Index)')
lines(CVARPort, col = 'blue')
legend('topleft',
       legend = c('Minimum-Variance', 'Minimum-CVaR'),
       col = c('black', 'blue'), lty = 1)

#relative performance
RelOutPerf <- (CVARPort - GMVPort) / GMVPort * 100
plot(RelOutPerf, type = 'h', col = 'blue', xlab = '',
     ylab = 'Percent',
     main = 'Relative Out-Performance Min-CVaR vs. Min-Variance')
abline(h = 0, col = 'grey')
