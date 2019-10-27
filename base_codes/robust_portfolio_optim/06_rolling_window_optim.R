##### code 006 #####
#previous: descriptive_statistics.R

#### (back-test) rolling window optimization - FRAPO 10.6

#defining a vector containing the estimator method's names
EST <- c('CovClassic', 'CovMcd', 'CovMest', 'CovMMest',
         'CovMve', 'CovOgk', 'CovSde', 'CovSest')

#defining a funtion for back-test with arguments ...
PfBack <- function(x, method = c('CovClassic', 'CovMcd', 'CovMest', 'CovMMest',
                                 'CovMve', 'CovOgk', 'CovSde', 'CovSest'), ...) {
  cov <- Moments(x, method = method) #function defined at mom_estimator_function.R
  return(PortMinVar(cov)) #function defined at min_var_optimization.R
}


#conducting back-test on portfolios
PfWeights <- lapply(EST, function(x) rollapply(rzoo, width = 120, FUN = PfBack,
                                               method = x, by.column = F,
                                               align = 'right'))

# ??????????
periods <- as.Date(index(PfWeights[[1]]))

#calculate portfolio returns / relative performance
PfReturns <- lapply(PfWeights, function(x) rowSums(lag(x, k = -1) * rzoo))
PfReturns <- zoo(matrix(unlist(PfReturns), ncol = length(PfReturns)), periods)
colnames(PfReturns) <- EST
PortOut <- (PfReturns[, -1] - PfReturns[, 1])

#plot relative performance
plot(PortOut, type = 'h',
     xlab = '',
     ylab = EST[-1],
     main = "Relative Performance",
     ylim = range(PortOut))

#statistics on relative performance
PortRelStats <- rbind(apply(PortOut, 2, summary),
                      skewness(PortOut)
                      )