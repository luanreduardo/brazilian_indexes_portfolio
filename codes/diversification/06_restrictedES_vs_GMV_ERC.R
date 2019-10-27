##### code 006 #####

####comparison of restricted ES portfolios with GMV and ERC

library(FRAPO)
library(PortfolioAnalytics)

#loading data and computing returns
data("MultiAsset")
R <- returnseries(MultiAsset, percentage = F, trim = T)
N <- ncol(R)

#defining constraints and objective for CVaR budget
C1 <- constraint_v1(assets = colnames(R), min = rep(0, N), #error solved adding '_v1' to fun
                 max = rep(1, N), min_sum = 1, max_sum = 1) #error: specify constraint type
ObjCVaR <- add.objective_v1(constraints = C1, type = 'risk',
                         name = 'ES', arguments = list(p = .95),
                         enabled = T)
ObjCVaRBudget <- add.objective_v1(constraints = ObjCVaR,
                               type = 'risk_budget',
                               name = 'ES', max_prisk = .2,
                               arguments = list(p = .95),
                               enable = T)
SolCVaRBudget <- optimize.portfolio_v1(R = R, #version problems, partially solved using _v1
                                    constraints = ObjCVaRBudget,
                                    optimize_method = 'DEoptim',
                                    itermax = 50,
                                    search_size = 20000,
                                    trace = T)
WCVarBudget <- SolCVaRBudget$weights
CVaRBudget <- ES(R, weights = WCVarBudget, p = .95,
                 portfolio_method = 'component')

#minimum CVaR concentration portfolio
ObjCVaRMinCon <- add.objective_v1(constraints = ObjCVaR,
                                  type = 'risk_budget',
                                  name = 'ES',
                                  min_concentration = T,
                                  arguments = list(p = .95),
                                  enabled =  T)
SolCVaRMinCon <- optimize.portfolio_v1(R = R, #version problems
                                       constraints = ObjCVaRMinCon,
                                       optimize_method = 'DEoptim',
                                       itermax = 50,
                                       search_size = 20000,
                                       trace = T)
WCVaRMinCon <- SolCVaRMinCon$weights
CVaRMinCon <- ES(R, weights = WCVaRMinCon, p = .95,
                 portfolio_method = 'component')

#GMV portfolio
WGMV <- Weights(PGMV(R, percentage = F))
CVaRGMV <- ES(R, weights = WGMV, p = .95,
              portfolio_method = 'component')

#ERC portfolio
WERC <- Weights(PERC(cov(R), percentage = F))
CVaRERC <- ES(R, weights = WERC, p = .95,
              portfolio_method = 'component')
