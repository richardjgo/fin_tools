
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI)
library (DEoptim)

# example from vignette
data(edhec)
returns <- edhec[,1:4]
colnames(returns) <- c("CA", "CTAG", "DS", "EM")
print(head(returns, 5))

# character vector of the funds names
fund.names <- colnames(returns)

# specify a portfolio objective 
pspec <- portfolio.spec(assets=fund.names)
print.default(pspec)

# Constraints: 1, added to the portfolio object
# add the full investment constraint that specifies weights sum to 1.
pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=1, max_sum=1)

# add box constraints
pspec <- add.constraint(portfolio = pspec, type="box", min=0.05, max=0.40) # for any asset, min 
# min and max specified per asset
pspec <- add.constraint(portfolio = pspec, type ="box", min = c(0.05, 0, 0.08, 0.1), max = c(0.4, 0.3, 0.7, 0.55))

# Asset group constraint
pspec <- add.constraint(portfolio = pspec, type ="group", groups = list(groupA = c(1,2,3), groupB=4), 
                        group_min=c(0.1,0.15), group_max=c(0.85, 0.55))

# target return constraint
pspec <- add.constraint(portfolio = pspec, type="return", return_target= 0.007) #user specified target mean return

# Print: concise view of portfolio and constraints added
print(pspec)
summary(pspec)

# Constraints: 2, specified as separate objects

# full investment constraint
weight_constr <- weight_sum_constraint(min_sum = 1, max_sum = 1)
box_constr <- box_constraint(assets=pspec$assets, min =0, max=1)
group_constr <- group_constraint(assets=pspec$assets, groups = list(c(1,2,3),4),
                                group_min=c(0.10,0.15), group_max=c(0.85, 0.55), group_labels=c("GroupA", "GroupB")
                                )
ret_constr <- return_constraint(return_target = 0.007)



### Objectives
# type:={return, risk, risk_budget, weight_concentration}  # risk = minimise, return = maximise
# name := corresponds to a function in R.
pspec <- add.objective(portfolio = pspec, type = 'risk', name='ETL', arguments= list(p=0.95))
pspec <- add.objective(portfolio = pspec, type='return', name='mean')

pspec <- add.objective(portfolio = pspec, type='risk', name='sd')

print(pspec)
summary(pspec)



###### EXAMPLE 2 ###########

R <- edhec[,1:4]
# set up simple portfolio with lev and box constraint
pspec<- portfolio.spec(assets=colnames(R))
pspec <- add.constraint(portfolio=pspec, type="leverage", min_sum=0.99, max_sum=1.01)
pspec<-add.constraint(portfolio=pspec,type="box", min=0, max=1)
#generate random portfolios using three methods
rp1 <- random_portfolios(portfolio=pspec, permutations=5000, rp_method='sample') #preferred balance method
rp2 <- random_portfolios(portfolio=pspec, permutations=5000, rp_method='simplex') # cluster at asset toyes
rp3 <- random_portfolios(portfolio=pspec, permutations=5000, rp_method='grid')
# show feasible portfolios in mean-StdDev space
tmp1.mean <-apply(rp1, 1, function(x) mean(R %*% x))
tmp1.StdDev <-apply(rp1, 1, function(x) StdDev(R=R, weights=x))

tmp2.mean <-apply(rp2, 1, function(x) mean(R %*% x))
tmp2.StdDev <-apply(rp2, 1, function(x) StdDev(R=R, weights=x))

tmp3.mean <-apply(rp3, 1, function(x) mean(R %*% x))
tmp3.StdDev <-apply(rp3, 1, function(x) StdDev(R=R, weights=x))

#plot feasible portfolios
plot(x=tmp1.StdDev, y=tmp1.mean, col="gray", main="Random Portfolio Methods", ylab="mean", xlab="StDev")
points(x=tmp2.StdDev, y=tmp2.mean, col="red", pch=2)
points(x=tmp3.StdDev, y=tmp3.mean, col="lightgreen", pch=5)
legend("bottomright", legend=c("sample", "simplex", "grid"),col=c("gray", "red", "lightgreen"), 
       pch=c(1,2,5), bty = "n")


fev <- 0:5
par(mfrow=c(2,3))
for(i in 1:length(fev)){
  rp <- rp_simplex(portfolio = pspec, permutations = 2000, fev=fev[i])
  tmp.mean <- apply(rp, 1, function(x) mean(R %*% x))
  tmp.StdDev <- apply(rp, 1, function(x) StdDev(R=R, weights=x))
  plot(x=tmp.StdDev, y=tmp.mean, main=paste("FEV = ", fev[i]), 
       ylab="mean" , xlab <-"StdDev", col=rgb(0,0,100, 50, maxColorValue=255))
  }
par(mfrow=c(1,1))

# Generate Figure 3

par(mfrow=c(1,2))
# simplex
rp_simplex <- random_portfolios(portfolio =pspec , permutations =2000, rp_method='simplex')
tmp.mean <- apply(rp_simplex, 1, function(x) mean(R %*% x))
tmp.StdDev <- apply(rp_simplex, 1, function(x) StdDev(R=R, weights=x))
plot(x=tmp.StdDev, y=tmp.mean, main="rp_method=simplex fev=0:5", 
     ylab="mean", xlab="StdDev", col=rgb(0,0,100, 50, maxColorValue=255))
#sample
rp_sample <- random_portfolios(portfolio =pspec , permutations =2000, rp_method='sample')
tmp.mean <- apply(rp_sample, 1, function(x) mean(R %*% x))
tmp.StdDev <- apply(rp_sample, 1, function(x) StdDev(R=R, weights=x))
plot(x=tmp.StdDev, y=tmp.mean, main="rp_method=sample fev=0:5", 
     ylab="mean", xlab="StdDev", col=rgb(0,0,100, 50, maxColorValue=255))
par(mfrow=c(1,1))





## section 6

library(DEoptim)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
data(edhec)
R <- edhec[, 1:6]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQMN", "ED")
funds <- colnames(R)
# create portfolio object with lev and box contraints
init <- portfolio.spec(assets=funds)
init<- add.constraint(portfolio=init, type="leverage", min_sum=0.99, max_sum=1.01)
init<- add.constraint(portfolio = init, type="box", min=0.05, max=0.65)
#objective to max mean return
maxret <- add.objective(portfolio=init, type="return", name="mean")
#run optimisation
opt_maxret <- optimize.portfolio(R=R, portfolio=maxret, optimize_method="ROI", trace=TRUE)
print(opt_maxret)
#chart
plot(opt_maxret, risk.col="StdDev", return.col="mean", main = "Maximum Return Optimisation", chart.assets=TRUE, 
     xlim=c(0, 0.05), ylim=c(0,0.0085))

# add min portfolio variance objective

minvar <- add.objective(portfolio=init, type="risk", name="var")
opt_minvar <- optimize.portfolio(R=R, portfolio=minvar, optimize_method = "ROI", trace=TRUE, traceDE=10)
print(opt_minvar)
#chart
plot(opt_minvar, risk.col="StdDev", return.col="mean", main = "Minimise Variance Optimisation", chart.assets=TRUE, 
     xlim=c(0, 0.05), ylim=c(0,0.0085))


## 6.4
## Quadratic Utility maximisation 
qu <- add.objective(portfolio=init, type="return", name="mean")
qu <- add.objective(portfolio=qu, type="risk", name="var", risk_aversion = 0.25) #risk aversion param for quad utility
opt_qu <- optimize.portfolio(R=R, portfolio=qu, optimize_method="ROI", trace=TRUE)
print(opt_qu)
plot(opt_qu, risk.col="StdDev", return.col="mean", main ="Quadratic Utility Optimization", chart.assets=TRUE,
     xlim=c(0,0.05), ylim=c(0,0.0085))

##TRACE = TRUE ... trace portfolios of optimisation, visualises feasible space of the portfolios.
# neighbors relative to optimal
##??plot(opt_meanETL, risk.col="ETL", return.col="mean", main="mean-ETL Optimisation", neighbors= 25)

# 6.8 Maximise mean return per unit ETL with ETL equal contrib to risk

init <- portfolio.spec(assets=funds)
init<- add.constraint(portfolio=init, type="leverage", min_sum=0.99, max_sum=1.01)
init<- add.constraint(portfolio = init, type="box", min=0.05, max=0.65)

eq_meanETL <- add.objective(portfolio = init, type="return", name="mean")
eq_meanETL <- add.objective(portfolio = eq_meanETL, type="risk", name="ETL", arguments = list(p=0.95))
eq_meanETL <- add.objective(portfolio = eq_meanETL, type="risk_budget", name="ETL", min_concentration =TRUE, 
                            arguments = list(p=0.95))

opt_eq_meanETL <- optimize.portfolio(R=R, portfolio=eq_meanETL, optimize_method = "DEoptim", search_size = 2000, 
                                     trace=TRUE, traceDE=5)

print(opt_eq_meanETL)
plot.new()
plot(opt_eq_meanETL, risk.col="ETL", return.col="mean", main="Risk Budget mean-ETL Optimizaton", xlim=c(0,0.12),
     ylim=c(0.005, 0.009))

chart.RiskBudget(opt_eq_meanETL, risk.type="percentage", neighbours=25)

# Compare opt_meanETL, opt_rb_meanETL, opt_eq_meanETL

# View the weights and objective measures of each optimization
extractWeights(opt_combine)

obj_combine <- # etc etc