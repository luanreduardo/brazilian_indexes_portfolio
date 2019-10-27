##### code 003 #####

####tail-dependence versus low-beta portfolio

library(FRAPO) #for data set
library(copula) #for Clayton copula

#loading data
data('INDTRACK6') #S&P 500, from FRAPO package

#market and asset returns
RM <- returnseries(INDTRACK6[1:260, 1], method = 'discrete', trim = T) #returns for index
RA <- returnseries(INDTRACK6[1:260, -1], method = 'discrete', trim = T) #returns for assets
Beta <- apply(RA, 2, function(x) cov(x, RM) / var(RM)) #beta value for the stocks
Tau <- apply(RA, 2, function(x) cor(x, RM, method = 'kendall')) #kendall's tau rank correlations

#clayton copula coefficience for lower tail dependence
ThetaC <- copClayton@iTau(Tau) 
LambdaL <- copClayton@lambdaL(ThetaC)

#selecting stocks and calculating the weights
IdxBeta <- Beta < median(Beta) #selection based on estimated beta below stock's beta
WBeta <- -1 * log(abs(Beta[IdxBeta])) #weights calculated by an inverse log scale
WBeta <- WBeta / sum(WBeta) * 100

#tail dependence and weights calculation
IdxTD <- LambdaL < median(LambdaL) #selecting based on tail dependence coef. below stock's median
WTD <- -1 * log(LambdaL[IdxTD]) #weights calculated by an inverse log scale
WTD <- WTD / sum(WTD) * 100
Intersection <- sum(names(WTD) %in% names(WBeta)) / length(WBeta) * 100

#out-of-sample performance
RMo <- returnseries(INDTRACK6[260:290, 1], method = 'discrete', percentage = F) + 1 #for index
RAo <- returnseries(INDTRACK6[260:290, -1], method = 'discrete', percentage = F) + 1 #for assets

#defining a benchmark
RMo[1] <- 100 
RMEquity <- cumprod(RMo) #cumulative product

#defining low beta
LBEquity <- RAo[, IdxBeta]
LBEquity[1, ] <- WBeta
LBEquity <- rowSums(apply(LBEquity, 2, cumprod))

#tail dependence
TDEquity <- RAo[, IdxTD]
TDEquity[1, ] <- WTD
TDEquity <- rowSums(apply(TDEquity, 2, cumprod))

#collecting results
y <- cbind(RMEquity, LBEquity, TDEquity)
