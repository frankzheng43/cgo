### test strategy
ra = position_return[,"return"] 
rb = position_return[,"SH300_return"]
rf = position_return[,"rf_rate"]

## Sharp ratio
SharpeRatio(R = ra, Rf = rf, FUN = "StdDev")
SharpeRatio(R = ra["2010-01-01/2013-01-01"], Rf = 0, FUN = "StdDev")
SharpeRatio.annualized(R = ra, Rf = rf)
SharpeRatio.annualized(R = ra["2010-01-01/2013-01-01"], Rf = rf)
# Adjusted Sharpe ratio of the return distribution
AdjustedSharpeRatio(R = ra, Rf = 0)

##
# Active Premium or Active Return
ActivePremium(Ra = ra, Rb = rb)
ActiveReturn(Ra = ra, Rb = rb)
# information ratio
InformationRatio(Ra = ra, Rb = rb)
##
# AverageDrawdown: the average depth of the observed drawdowns
AverageDrawdown(R = ra)
# AverageLength: the average length (in periods) of the observed drawdowns
AverageLength(R = ra)
# AverageRecovery: the average length (in periods) of the observed recovery period.
AverageRecovery(R = ra)
## CAPM
table.CAPM(Ra = ra, Rb = rb, Rf =rf)
