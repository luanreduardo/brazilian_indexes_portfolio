##### code 003 #####

####comparison draw-down and GMV portfolio

library(fPortfolio) #for construction resources
library(FRAPO) #for data set
library(PerformanceAnalytics) #for analysis resources

data("MultiAsset") #from FRAPO (equity and fixed income indexes plus gold, monthly, 2004/2011)

#time series of discrete returns
Rets <- returnseries(MultiAsset, method = 'discrete',
                     percentage = F, trim = T)
Rets <- timeSeries(Rets, charvec = rownames(Rets))

#benchmark portfolio - GMV
gmvspec <- portfolioSpec()
GMV <- minvariancePortfolio(data = Rets, spec = gmvspec,
                            constraints = 'LongOnly')
GMVret <- timeSeries(Rets %*% getWeights(GMV), #time series of portfolio returns
                     charvec = time(Rets))
GMVDD <- PerformanceAnalytics::Drawdowns(GMVret) #historical draw-drowns

#plot of draw-downs for GMV
ylims <- c(-6, 0)
plot(GMVDD * 100, xlab = '', ylab = 'Draw-downs (percentage)',
     main = 'Draw-downs of Global Minimum Variance',
     ylim = ylims)
abline(h = 0, col = 'grey')
grid()

#max draw-down of GMV
GMVMaxDD <- max(-1.0 * GMVDD)

#computing CDaR 95% draw-downs from GMV draw-downs
MaxDD <- PMaxDD(MultiAsset, MaxDD = GMVMaxDD)
AveDD <- PAveDD(MultiAsset, AveDD = GMVMaxDD)
CDaR95 <- PCDaR(MultiAsset, alpha = .95, bound = GMVMaxDD)
CDaRMin95 <- PCDaR(MultiAsset, alpha = .95)

#plot of draw-downs
oldpar <- par(no.readonly = T)
par(mfrow = c(2, 2))

plot(AveDD, main = '(a) AveDD')
plot(MaxDD, ylim = ylims, main = '(b) MaxDD')
plot(CDaR95, ylim = ylims, main = '(c) CDaR')
plot(CDaRMin95, ylim = ylims, main = '(d) Minimum CDaR')

par(oldpar)