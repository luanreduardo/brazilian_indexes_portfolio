##### code 004 #####

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
