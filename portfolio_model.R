
#### Generates portfolio

library(xts)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI)
library (DEoptim)

file <- "~/GCS/returns_Q216.csv"
returns_Q216 <- read_csv(file=file)
returns <- read.zoo(data.frame(returns_Q216), FUN = as.Date, format='%d/%m/%Y')

colnames(returns) <- c("UK.EQUITY", "EU.EQUITY", "NA.EQUITY", "ASIA.EQUITY", "JPN.EQUITY", "EM.EQUITY", 
                       "WORLD.EQUITY.EX.UK", "ABS.RETURN", "PROPERTY", "GILTS", "GBP.CREDIT", "LIBOR",
                       "LIBOR.PLUS.3", "GLOBAL.AGG", "GLOBAL.IG", "EM.DEBT", "GLOBAL.HY", "GBP.ALT.CREDIT")
print(head(returns, 5))

# character vector of the funds names
asset.class.names <- colnames(returns)

# specify a portfolio objective 
p.init <- portfolio.spec(assets=asset.class.names)
print.default(pspec)

# Constraints: 1, added to the portfolio object
# add the full investment constraint that specifies weights sum to 1.
p_spec <- add.constraint(portfolio=p.init, type="weight_sum", min_sum=1, max_sum=1)


# Asset group constraint
pspec <- add.constraint(portfolio = pspec, type ="group", groups = list(groupA = c(1,2,3), groupB=4), 
                        group_min=c(0.1,0.15), group_max=c(0.85, 0.55))








