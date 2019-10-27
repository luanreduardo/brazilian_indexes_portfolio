##### code 005 #####

####key measures of portfolio solutions

library(PerformanceAnalytics) #tools for measuring performance

#key measures (ex post)
RAdec <- RA / 100
RALB <- RAdec[, names(WBeta)] #Lb stands for low beta
RATD <- RAdec[, names(WTD)] #Td stands for tail dependence

LbStd <- StdDev(rowSums(RALB * WBeta / 100)) * 100 #standard deviation
TdStd <- StdDev(rowSums(RATD * WTD / 100)) * 100 

LbES95 <- abs(ES(R = rowSums(RALB * WBeta / 100), #expected shortfall
                 method = 'gaussian')) * 100
TdES95 <- abs(ES(R = rowSums(RATD * WTD / 100),
                 method = 'gaussian')) * 100

LbDr <- dr(WBeta, Sigma = cov(RALB)) #diversification ratio
TdDr <- dr(WTD, Sigma = cov(RATD))

LbCr <- cr(WBeta, Sigma = cov(RALB)) #concentration ratio
TdCr <- cr(WTD, Sigma = cov(RATD))

#key measures (ex ante)
LbRetO <- returnseries(LBEquity, method = 'discrete', #update: percent -> percentage
                       percentage = F, trim = T)
TdRetO <- returnseries(TDEquity, method = 'discrete',
                       percentage = F, trim = T)

BmRetO <- RMo[-1] - 1

timeArtificial <- timeSequence(from = '1997-03-01', by = '7d',
                               length.out = length(LbRetO))
LbRetO <- timeSeries(LbRetO, as.Date(timeArtificial))
TdRetO <- timeSeries(TdRetO, as.Date(timeArtificial))
BmRetO <- timeSeries(BmRetO, as.Date(timeArtificial))

km <- function(pf, bm, scale = 52) {
  
  ra <- Return.annualized(pf, scale = scale) * 100
  ap <- ActivePremium(pf, bm, scale = scale)
  te <- sd(pf - bm) * sqrt(scale)
  
  ir <- ap / te
  
  upr <- UpDownRatios(pf, bm, method = 'Capture', side = 'Up')
  dnr <- UpDownRatios(pf, bm, method = 'Capture', side = 'Down')
  
  res <- c(ra, ir, upr, dnr)
  names(res) <- c('Return', 'IR', 'UpRatio', 'DownRatio')
  
  return(res)
}

LbKM <- km(LbRetO, BmRetO)
TdKM <- km(TdRetO, BmRetO)
TdKM