##### code 001 #####

####comparison of portfolio solution by sectors

library(FRAPO) #for the functions used
library(fPortfolio) #for the datasets
library(lattice) #for data plotting

#loading data and calculating returns
data("SPISECTOR")
Idx <- interpNA(SPISECTOR[, -1], method = 'before') #interpolation of NA values
R <- returnseries(Idx, method = 'discrete', trim = T) #discrete daily returnos
V <- cov(R) #covariance matrix

#allocation weights for portfolio optimizations
GMVw <- Weights(PGMV(R)) #global-minimum variance
MDPw <- Weights(PMD(R)) #most-diversified portfolio
MTDw <- Weights(PMTD(R)) #minimum tail-dependent
ERCw <- Weights(PERC(V)) #equal-risk contributed

#combining solutions
W <- cbind(GMVw, MDPw, MTDw, ERCw) #matrix of weights 
MRC <- apply(W, 2, mrc,  Sigma = V) #marginal risk contribution for each weight vector
rownames(MRC) <- colnames(Idx)
colnames(MRC) <- c('GMV', 'MDP', 'MTD', "ERC")

#plot allocations for each portfolio relative to GMV
oldpar <- par(no.readonly = T)
par(mfrow = c(2, 2))

dotchart(GMVw, xlim = c(0, 40), main = 'GMV Allocation', pch = 19)

dotchart(MDPw - GMVw, xlim = c(-20, 20), main = 'MDP vs. GMV', pch = 19)
abline(v = 0, col = 'grey')

dotchart(MTDw - GMVw, xlim = c(-20, 20), main = 'MTD vs. GMV', pch = 19)
abline(v = 0, col = 'grey')

dotchart(ERCw - GMVw, xlim = c(-20, 20), main = 'ERC vs. GMV', pch = 19)
abline(v = 0, col = 'grey')

par(oldpar)

#plotting the marginal risk contribution for each portfolio, using lattice package
Sector <- factor(rep(rownames(MRC), 4), levels = sort(rownames(MRC))) #defininf the factors
Port <- factor(rep(colnames(MRC), each = 9), levels = colnames(MRC))
MRCdf <- data.frame(MRC = c(MRC), Port, Sector) #defining the dataframe

dotplot(Sector ~ MRC | Port, groups = Port, data = MRCdf,
        xlab = 'Percentages',
        main = 'MR Contributions by Sector per Portfolio',
        col = 'black', pch = 19)

dotplot(Port ~ MRC | Sector, groups = Sector, data = MRCdf,
        xlab = 'Percentages',
        main = 'MR Contributions by Portfolio per Sector',
        col = 'black', pch = 19)