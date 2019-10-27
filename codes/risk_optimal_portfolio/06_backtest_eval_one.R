##### code 006 #####

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