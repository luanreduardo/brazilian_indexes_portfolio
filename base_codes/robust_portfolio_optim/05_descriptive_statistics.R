##### code 005 #####
#previous: min_var_optimization.R

#### (back-test) descriptive statistics of returns - FRAPO 10.5

#packages
install.packages(c('FRAPO', 'PerformanceAnalytics', 'quadprog', 'rrcov', 'zoo'))
library(FRAPO) #repository for Pfaff's book, containing data samples
library(PerformanceAnalytics) #functions for financial performance and risk analysis
library(quadprog) #routines and documentation for solving quadratic programming problems
library(rrcov) #package dedicated to robust multivariate statistics
library(zoo) #for dealing with zoo objects

#loading data from FRAPO
data(StockIndex)
#defining the price zoo object
pzoo <- zoo(StockIndex, order.by = rownames(StockIndex))
#calculating the returns zoo object
rzoo <- (pzoo / lag(pzoo, k = -1) - 1) * 100

#descriptive statistics
boxplot(coredata(rzoo))
rstats <- rbind(apply(rzoo, 2, summary),
                skewness(rzoo),
                kurtosis(rzoo))
#plots boxplot and a table for summary, skewness and kurtosis of the data set