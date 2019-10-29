##### code 009 #####

####risk surface plot of multi-asset portfolio, part two
###surface plot with superimposed mdp solutions

#per standard deviation risk level
surfacePlot(surf, type = 'filled.contour',
            palette = topo.colors, addHull = T,
            addGrid = F, addAssets = F,
            xlab = 'Target Risk', ylab = 'Target Return',
            main = 'Convex Hull with Risk Surface:\nStd.Dev. of MRC and MDP-line')
lines(x = dropt[, 2], y = dropt[, 1], col = 'blue', lwd = 2)
box()

#computing special points and plotting
frontier <- portfolioFrontier(data)
MVP <- minvariancePoints(frontier)
TGP <- tangencyPoints(frontier)
sap <- singleAssetPoints(frontier)

wewp <- rep(1/NAssets, NAssets)
mewp <- crossprod(mu, wewp)
sewp <- sqrt(crossprod(wewp, Sigma) %*% wewp)
ERC <- PERC(Sigma)

werc <- Weights(ERC) / 100.0
merc <- crossprod(mu, werc)
serc <- sqrt(crossprod(werc, Sigma) %*% werc)

points(sap, col = 'darkgreen', pch = 19, cex = .8)
text(sap, ANames, col = 'darkred', cex = .6, pos = 4)
points(TGP, col = 'tan', pch = 19, cex = 2.5)
text(TGP[1],  TGP[2], 'TGP', col = 'purple', cex = .5)
points(x = sewp, y = mewp, col = 'tan', pch = 19, cex = 2.5)
text(sewp, mewp, 'EWP', col = 'purple', cex = .5)
points(x = serc, y = merc, col = 'tan', pch = 19, cex = 2.5)
text(serc, merc, 'ERC', col = 'purple', cex = .5)
points(MVP, col = 'tan', pch = 19, cex = 2.5)
text(MVP[1], MVP[2], 'MVP', col = 'purple', cex = .5)
