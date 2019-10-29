#code 02

####comparison draw-down and GMV portfolio

library(fPortfolio) #for construction resources
library(FRAPO) #for data set
library(PerformanceAnalytics) #for analysis resources


#time series of discrete returns
Rets <- returnseries(br_indexes, method = 'discrete',
                     percentage = F, trim = T) #in ts_br_returns, trim = F
Rets <- timeSeries(Rets, charvec = rownames(Rets))

#benchmark portfolio - GMV
gmvspec <- portfolioSpec()
GMV <- minvariancePortfolio(data = Rets, spec = gmvspec,
                            constraints = 'LongOnly')
GMVret <- timeSeries(Rets %*% getWeights(GMV), #time series of portfolio returns
                     charvec = time(Rets))
GMVDD <- PerformanceAnalytics::Drawdowns(GMVret) #historical draw-drowns

#plot of draw-downs for GMV
ylims <- c(-25, 0)
plot(GMVDD * 100, xlab = '', ylab = 'Draw-downs (percentage)',
     main = 'Draw-downs Global Minimum-Variance',
     ylim = ylims)
abline(h = 0, col = 'grey')
grid()

dev.copy(png, 'results/drawdowns_GMV.png')
dev.off()

#max draw-down of GMV
GMVMaxDD <- max(-1.0 * GMVDD)

#computing CDaR 95% draw-downs from GMV draw-downs TAKES TIME
MaxDD <- PMaxDD(br_indexes, MaxDD = GMVMaxDD)
AveDD <- PAveDD(br_indexes, AveDD = GMVMaxDD)
CDaR95 <- PCDaR(br_indexes, alpha = .95, bound = GMVMaxDD)
CDaRMin95 <- PCDaR(br_indexes, alpha = .95)



#plot of draw-downs
oldpar <- par(no.readonly = T)
par(mfrow = c(2, 2))

plot(AveDD, main = '(a) AveDD')
plot(MaxDD, ylim = ylims, main = '(b) MaxDD')
plot(CDaR95, ylim = ylims, main = '(c) CDaR')
plot(CDaRMin95, ylim = ylims, main = '(d) Minimum CDaR')

dev.copy(png, 'results/drawdowns_&_CDaR.png')
dev.off()

par(oldpar)

####analysis of portfolio solutions

#names for the 5 portfolio types
Pnames <- c('GMV', 'MaxDD', 'AveDD', 'CDaR95', 'CDaRMin95')

#creating a matrix with all portfolio allocations
WeightMatrix <- cbind(getWeights(GMV),
                      Weights(MaxDD),
                      Weights(AveDD),
                      Weights(CDaR95),
                      Weights(CDaRMin95))
colnames(WeightMatrix) <- Pnames

#expected shortfall and components
tmp <- apply(WeightMatrix, 2, function(x) ES(Rets, weights = x,
                                             method = 'gaussian',
                                             portfolio_method = 'component'))

#ES 95%
PES <- unlist(lapply(tmp, function(x) x[[1]])) * 100

#marginal contributions to ES
PMES <- matrix(unlist(lapply(tmp, function(x) x[[3]])),
               nrow = ncol(Rets)) * 100
rownames(PMES) <- colnames(Rets)
colnames(PMES) <- Pnames

#marginal contributions to standard deviation
V <- cov(Rets)
PMRC <- apply(WeightMatrix, 2, mrc, Sigma = V)
rownames(PMRC) <- colnames(Rets)

#diversification ratio
PDR <- apply(WeightMatrix, 2, dr, Sigma = V)

### SET A SINGLE TABLE AND SAVE
portfolio_analysis <- list()
portfolio_analysis[[1]] <- PES
portfolio_analysis[[2]] <- PMES
portfolio_analysis[[3]] <- PMRC
portfolio_analysis[[4]] <- PDR
saveRDS(portfolio_analysis, file = 'results/portfolio_analysis.RDS')
