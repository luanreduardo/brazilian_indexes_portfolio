##### code 004 #####

####plotting wealth progression and relative performance

#time series plots of equity curves
plot(RMEquity, type = 'l', ylim = range(y),
     ylab = 'Equity Index',
     xlab = 'Out-of-Sample Periods')
lines(LBEquity, lty = 2, col = 'blue')
lines(TDEquity, lty = 3, col = 'red')
legend('topleft',
       legend = c('S&P500', 'Low Beta', 'Lower Tail Dep.'),
       lty = 1:3, col = c('black', 'blue', 'red'))

#barplot of relative performance
RelOut <- rbind((LBEquity / RMEquity - 1) * 100,
                (TDEquity / RMEquity - 1) * 100)
RelOut <- RelOut[, -1]
barplot(RelOut, beside = T, ylim = c(-5, 17),
        names.arg = 1:ncol(RelOut),
        legend.text = c('Low Beta', 'Lower Tail Dep.'),
        args.legend = list(x = 'topleft'))
abline(h = 0)

box()
