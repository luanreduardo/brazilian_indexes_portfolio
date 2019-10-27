##### code 007 #####

####backtest evaluation, part 02

#VaR
MVVAR <- -100 * VaR(MVRet, p = .95, method = 'gaussian')
CDVAR <- -100 * VaR(CDRet, p = .95, merhod = 'gaussian')

#ES
MVES <- -100 * ES(MVRet, p = .95, method = 'gaussian')
CDES <- -100 * ES(CDRet, p = .95, method = 'gaussian')

#sharpe
MVSR <- SharpeRatio(MVRet)
CDSR <- SharpeRatio(CDRet)

#annualised returns
MVRA <- Return.annualized(MVRet, scale = 52)
CDRA <- Return.annualized(CDRet, scale = 52)

#draw-downs
MVDD <- -100 * findDrawdowns(MVRet)$return
MVDD <- MVDD[MVDD != 0.0]
length(MVDD)
summary(MVDD)
CDDD <- -100 * findDrawdowns(CDRet)$return
CDDD <- CDDD[CDDD != 0.0]
length(CDDD)
summary(CDDD)