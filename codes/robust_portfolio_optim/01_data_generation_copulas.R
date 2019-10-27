##### code 001 #####

#### generating data through Gauss copula (normal and t margins) 
#### and Student's t copula (t margin) - FRAPO 10.1

#install.packages(c('cccp', 'gsl', 'copula', 'rrcov'))
library(cccp) #suitable for solving math programs with second-order constrains
library(copula) #general package for dealing with copulae
library(rrcov) #package dedicated to robust multivariate statistics

#defines a normal (ncop) and a t (tcop) copulae
ncop <- normalCopula(param = .5, dim = 5)
tcop <- tCopula(param = .5, dim = 5, df = 5, df.fixed = TRUE)

#creating the data generation processes
#gaussian with normally distributed margins
NcopMargN <- mvdc(ncop, margins = 'norm',
                  paramMargins = list(list(mean = 0, sd = 1)),
                  marginsIdentical = T)
#gaussian with t distributed margins
NcopMargT <- mvdc(ncop, margins = "t",
                  paramMargins = list(df = 5),
                  marginsIdentical = T)
#student's t with t distributed margins
TcopMargT <- mvdc(tcop, margins = 't',
                  paramMargins = list(df = 5),
                  marginsIdentical = T)

#list objects for data generations processes
Lobj <- list()
length(Lobj) <- 1000

set.seed(27272)

#generating random samples
#gaussian normal margins
rNcopMargN <- lapply(Lobj, function(x) rMvdc(240, NcopMargN))
#gaussian t margins
rNcopMargT <- lapply(Lobj, function(x) rMvdc(240, NcopMargT))
#student's t t margins
rTcopMargT <- lapply(Lobj, function(x) rMvdc(240, TcopMargT))

#each of this lists are consisted of 1000 samples with 240 rows and 5 columns
#for fictional asset returns