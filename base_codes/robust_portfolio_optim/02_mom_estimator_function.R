##### code 002 #####
#previous: data_generation_copulas

#### function for estimating moments - FRAPO 10.2

#defining a function for moment estimation
#the arguments are a random data set, a kind of estimator and the ellipsis argument
Moments <- function(x, method = c('CovClassic', 'CovMcd',
                                  'CovMest', 'CovMMest',
                                  'CovMve', 'CovOgk',
                                  'CovSde', 'CovSest'), ...){
  method <- match.arg(method) #matches arg against a table of candidate values specified by 'choices'
  ans <- do.call(method, list(x = x, ...))
  return(getCov(ans))
}

#this function can be used to estimate the second moment for each
#of the list elements of the DGPs and sample sizes