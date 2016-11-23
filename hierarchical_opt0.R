
### my hash at optim with hierarch


## http://rossb34.github.io/PortfolioAnalyticsPresentation2016/#29
## Hierarchical Optimisation

# HO: Naive Optimisation



library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI)
library(DEoptim)
library(quantmod)


# note on solvers
# Linear/Quad:   ROI, GLPK, Symp, Quadprog
# Global (stoch/contin): random portfolios, differential evolution, particle swarm, gen simulated annealing.

data(edhec)
R <- edhec[,1:4]
colnames(R) <- c("CA","CTAG", "DS","EM")
head(R,5)

portf.naive <- portfolio.spec(colnames(R))
portf.naive <- add.constraint(portf.naive, type="weight_sum",
                              min_sum=-0.05, max_sum=0.05)
portf.naive <- add.constraint(portf.naive, type="box", min=-0.5, max=0.5)
portf.naive <- add.constraint(portf.naive, type = "leverage_exposure", leverage=2)
portf.naive <- add.objective(portf.naive, type="risk", name="StdDev")
portf.naive <- add.objective(portf.naive, type="risk_budget",
                             name="StdDev", max_prisk=0.50)




#####################
#base#



# define base portfolio
data(edhec)
R <- edhec
colnames(R) <- c("CA","CTAG", "DS","EM", "EqMN", "FI Arb", "GM", "LS Eq", "M Arb", "RV", "SS", "FF")

R <- edhec[,1:8]
colnames(R) <- c("CA","CTAG", "DS","EM", "EqMN", "FI Arb", "GM", "LS Eq")

# Simple efficient frontier

portf.base <- portfolio.spec(colnames(R))
portf.base <- add.constraint(portf.base, type="weight_sum",
                             min_sum=0.99, max_sum=1.01)
portf.base <- add.constraint(portf.base, type="box", min=0, max=1)
ef <- create.EfficientFrontier(R, portfolio=portf.base, 
                               type="mean-StdDev", n.portfolios=100)
chart.EfficientFrontier(ef, match.col="StdDev", pch=18, col="lightblue")

summary(ef)

# entire feasible space
R <- edhec
colnames(R) <- c("CA","CTAG", "DS","EM", "EqMN", "FI Arb", "GM", "LS Eq", "M Arb", "RV", "SS", "FF")

p <- portfolio.spec(colnames(R))
p <- add.constraint(p, type="weight_sum", min_sum=0.99, max_sum=1.01)
p <- add.constraint(p, type="box", min=0, max=1)
p <- add.objective(p, type="return", name="mean", multiplier=0)
p <- add.objective(p, type="risk", name="StdDev", multiplier=0)
rp <- random_portfolios(p, permutations=5000, rp_method='sample')
opt <- optimize.portfolio(R, p, optimize_method="random", rp=rp, trace=TRUE)
xt <- extractStats(opt)





