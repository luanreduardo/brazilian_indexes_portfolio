##### code 002 #####

####key measures for portfolio solutions

library(PerformanceAnalytics)

#portfolio risk measures and characteristics
#using apply in each vector of the weights matrix
Rdec <- R / 100
Pret <- apply(W, 2, function(x) Rdec %*% x / 100)
SD <- apply(Pret, 2, sd) * 100 #standard deviation
ES95 <- apply(Pret, 2, function(x) 
  abs(ES(R = x, method = 'modified') * 100)) #expected shortfalls at 95%
DR <- apply(W, 2, dr, Sigma = V) #diversification ratio
CR <- apply(W, 2, cr, Sigma = V) #concentration ratio

#summarizing results
Res <- rbind(SD, ES95, DR, CR)
