

#  from Ross's R code #1


library(quantmod)


# data prep
# https://github.com/rossb34/PortfolioAnalyticsPresentation2016/blob/master/data_prep.R

md <- new.env()
# SP500 ETF as a proxy of the market
mkt.sym <- "SPY"
# sector ETFs
sec.sym <- c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
# commodity ETFs
# com.sym <- c("GLD", "SLV", "USO", "DBA", "UNG")
# symbols <- c(mkt.sym, eq.sym, com.sym)
symbols <- c(mkt.sym, sec.sym)
getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from='1999-01-01', env = md)

for(symbol in symbols) {
  x <- md[[symbol]]
  x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)
  indexFormat(x) <- '%Y-%m-%d'
  colnames(x) <- gsub("x",symbol,colnames(x))
  md[[symbol]] <- x
  
}

ret <- na.omit(Return.calculate(do.call(cbind, eapply(md, function(x) Ad(x))), "discrete"))
colnames(ret) <- gsub("\\.[^.]*$", "", colnames(ret))
# sector and market returns
R.sector <- ret[,sec.sym]
R.mkt <- ret[, mkt.sym]
# save the sector etf returns and the market data environment
save(R.sector, file="data/sector.rda")
save(R.mkt, file="data/market.rda")
save(md, file="data/md_env.RData")



# charts

chart.RiskReturnScatter(R.sector, main = "Sector ETFs Annualized Return and Risk")


## heirarchical optimisation

R<- R.sector

portf.naive <- portfolio.spec(colnames(R))
portf.naive <- add.constraint(portf.naive, type="weight_sum",
                              min_sum=-0.05, max_sum=0.05)
portf.naive <- add.constraint(portf.naive, type="box", min=-0.5, max=0.5)
portf.naive <- add.constraint(portf.naive, type = "leverage_exposure", leverage=2)
portf.naive <- add.objective(portf.naive, type="risk", name="StdDev")
portf.naive <- add.objective(portf.naive, type="risk_budget",
                             name="StdDev", max_prisk=0.50)

rp.naive <- random_portfolios(portf.naive, permutations=1000, rp_method='sample')
opt.naive <- optimize.portfolio.rebalancing(R, portf.naive,
                                            optimize_method="random",
                                            rebalance_on="quarters",
                                            training_period=36,
                                            rp=rp.naive, trace=TRUE)
# compute arithmetic portfolio returns because of negative weights
ret.naive <- Return.portfolio(R, extractWeights(opt.naive), geometric = FALSE)
colnames(ret.naive) <- "naive.ls"
charts.PerformanceSummary(ret.naive)


# long portfolio
p.long <- portfolio.spec(assets=colnames(R))
p.long <- add.constraint(p.long, type="weight_sum", min_sum=0.99, max_sum=1.01)
p.long <- add.constraint(p.long, type="box", min=0, max=0.85)
p.long <- add.objective(p.long, type="risk", name="StdDev")
p.long <- add.objective(p.long, type="risk_budget", name="StdDev", max_prisk=0.50)
rp.long <- random_portfolios(p.long, permutations=1000, rp_method='sample')
# short portfolio
p.short <- portfolio.spec(assets=colnames(R))
p.short <- add.constraint(p.short, type="weight_sum", min_sum=-1.01, max_sum=-0.99)
p.short <- add.constraint(p.short, type="box", min=-0.85, max=0)
p.short <- add.objective(p.short, type="risk", name="StdDev")
p.short <- add.objective(p.short, type="risk_budget", name="StdDev", max_prisk=0.50)
rp.short <- random_portfolios(p.short, permutations=1000, rp_method='sample')


## Specify Portfolio Heirerarchy
# combined portfolio
p <- portfolio.spec(assets=paste("proxy",1:2, sep="."))
p <- add.constraint(p, type="weight_sum", min_sum=0.99, max_sum=1.01)
p <- add.constraint(p, type="box", min=0.1, max=1)
p <- add.objective(p, type="return", name="mean")
p <- add.objective(p, type="risk", name="StdDev")
rp <- random_portfolios(p, permutations=1000, rp_method='sample')
mult.portf <- mult.portfolio.spec(p)
mult.portf <- add.sub.portfolio(mult.portf, p.long, rp=rp.long, 
                                optimize_method="random", rebalance_on="quarters", 
                                training_period=36)
mult.portf <- add.sub.portfolio(mult.portf, p.short, rp=rp.short, 
                                optimize_method="random", rebalance_on="quarters", 
                                training_period=36)
# run the optimization
opt.m.ls <- optimize.portfolio.rebalancing(R, mult.portf,
                                           optimize_method = "random",
                                           trace = TRUE, rp = rp,
                                           rebalance_on = "quarters",
                                           training_period = 36)
r.ls <- Return.portfolio(opt.m.ls$R, extractWeights(opt.m.ls), geometric = FALSE)
charts.PerformanceSummary(r.ls)


###############################################
## new for EDHEC 1:8


# helper function to find the max sharpe ratio portfolio given the output from extract stats
find.max.sr <- function(x){
  x[which.max(x[,"mean"] / x[,"StdDev"]),]
}

#source("data_prep.R")

# in and out sample
R <- edhec["/2014",1:8]
R.os <- edhec["2015/",1:8]
x.assets <- StdDev(R)
y.assets <- colMeans(R)

# set up chart ranges
x.lower <- min(0, min(x.assets) * 0.9)
x.upper <- max(x.assets) * 1.1
y.lower <- min(0, min(y.assets) * 0.9)
y.upper <- max(y.assets) * 1.1

# define base portfolio
portf.base <- portfolio.spec(colnames(R))
# the only thing I will be changing is constraints so define all the objectives
# set multiplier of 0 so the values are calculated, but no optimization is done
portf.base <- add.objective(portf.base, type="return", name="mean", multiplier=0)
portf.base <- add.objective(portf.base, type="risk", name="StdDev", multiplier=0)
portf.base <- add.objective(portf.base, type="risk", name="ES", arguments=list(p=0.9), multiplier=0)

my_colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")


##### Example 4: Full Investment, Long Only Box and Position Limit Constraints #####
p4 <- portf.base
p4 <- add.constraint(p4, type="weight_sum",
                     min_sum=0.99, max_sum=1.01)
p4 <- add.constraint(p4, type="box", min=0, max=1)
p4 <- add.constraint(p4, type="position_limit", max_pos=2)

# generate random portfolios for the baseline portfolio
rp4 <- random_portfolios(p4, permutations=5000, rp_method="sample")

opt4 <- optimize.portfolio(R, p4, optimize_method="random", rp=rp4, trace=TRUE)
xt4 <- extractStats(opt4)
p4.mean <- xt4[,"mean"]
p4.sd <- xt4[,"StdDev"]
p4.es <- xt4[,"ES"]
opt.xt4 <- find.max.sr(xt4)

png("figures/p4_ef.png", width = w, height = h, units = "px")
plot(x=x.assets, y=y.assets, type="n", main="Feasible Space",
     xlim=c(x.lower, x.upper), ylim=c(y.lower, y.upper),
     ylab="mean", xlab="StdDev", cex.axis=0.8)

# baseline portfolio feasible space
points(x=p1.sd, y=p1.mean, col=my_colors[2], pch=1)
points(x=p4.sd, y=p4.mean, col=my_colors[3], pch=1)
# assets
points(x=x.assets, y=y.assets, col="black", pch=19)
text(x=x.assets, y=y.assets, labels=colnames(R), pos=4, cex=0.8)
# max sharpe ratio portfolio
points(x=opt.xt4["StdDev"], y=opt.xt4["mean"], col="orange", pch=19)
text(x=opt.xt4["StdDev"], y=opt.xt4["mean"], pos=2, cex=0.8,
     labels=paste("Max SR =",round(opt.xt4["mean"]/opt.xt4["StdDev"],4)), col="black")
legend("topleft", legend = c("Portfolio 1", "Portfolio 4"), bty="n",
       pch = c(1, 1), col = c(my_colors[2], my_colors[3]))
dev.off()


######
# Section 3 
# with hierarchical opt










