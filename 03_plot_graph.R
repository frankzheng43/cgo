## plotting
# overall
charts.PerformanceSummary(R = ra, Rf = rf, main = "From 2004-06-02 to 2017-04-17")
chart.CumReturns(position_return, legend.loc = "topleft", main = "From 2004-06-02 to 2017-04-17")
capm_table <- table.CAPM(Ra = ra, Rb = rb, Rf = rf) 
SharpeRatio.annualized(R = ra, Rf = rf)
chart.Histogram(R = ra, main = "Return distribution")
calendar_return <- table.CalendarReturns(ra)
drawdowns <- table.Drawdowns(ra)
chart.Drawdown(R = ra, main = "Drawdowns")
# simple hedge


# bull periods
chart.CumReturns(position_return["2006-01/2007-11",1:2], legend.loc = "topleft", main = "From 2006-01 to 2007-11")
capm_table <- table.CAPM(Ra = ra["2006-01/2007-11"], Rb = rb["2006-01/2007-11"], Rf = rf["2006-01/2007-11"]) 
SharpeRatio.annualized(R = ra["2006-01/2007-11"], Rf = rf["2006-01/2007-11"])
chart.CumReturns(position_return["2014-04/2015-04",1:2], legend.loc = "topleft", main = "From 2014-04 to 2015-04")
capm_table <- table.CAPM(Ra = ra["2014-04/2015-04"], Rb = rb["2014-04/2015-04"], Rf = rf["2014-04/2015-04"]) 
SharpeRatio.annualized(R = ra["2014-04/2015-04"], Rf = rf["2014-04/2015-04"])
# bear periods
chart.CumReturns(position_return["2008-01/2009-02",1:2], legend.loc = "topleft", main = "From 2008-01 to 2009-02")
capm_table <- table.CAPM(Ra = ra["2008-01/2009-02"], Rb = rb["2008-01/2009-02"], Rf = rf["2008-01/2009-02"]) 
SharpeRatio.annualized(R = ra["2008-01/2009-02"], Rf = rf["2008-01/2009-02"])
chart.CumReturns(position_return["2011-07/2012-11",1:2], legend.loc = "topleft", main = "From 2011-07 to 2012-11")
capm_table <- table.CAPM(Ra = ra["2011-07/2012-11"], Rb = rb["2011-07/2012-11"], Rf = rf["2011-07/2012-11"]) 
SharpeRatio.annualized(R = ra["2011-07/2012-11"], Rf = rf["2011-07/2012-11"])
chart.CumReturns(position_return["2015-07/2016-05",1:2], legend.loc = "topleft", main = "From 2015-07 to 2016-05")
capm_table <- table.CAPM(Ra = ra["2015-07/2016-05"], Rb = rb["2015-07/2016-05"], Rf = rf["2015-07/2016-05"]) 
SharpeRatio.annualized(R = ra["2015-07/2016-05"], Rf = rf["2015-07/2016-05"])
# uncertain periods
chart.CumReturns(position_return["2009-07/2011-06",1:2], legend.loc = "topleft", main = "From 2009-07 to 2011-06")
capm_table <- table.CAPM(Ra = ra["2009-07/2011-06"], Rb = rb["2009-07/2011-06"], Rf = rf["2009-07/2011-06"]) 
SharpeRatio.annualized(R = ra["2009-07/2011-06"], Rf = rf["2009-07/2011-06"])
chart.CumReturns(position_return["2012-12/2014-05",1:2], legend.loc = "topleft", main = "From 2012-12 to 2014-05")
capm_table <- table.CAPM(Ra = ra["2012-12/2014-05"], Rb = rb["2012-12/2014-05"], Rf = rf["2012-12/2014-05"]) 
SharpeRatio.annualized(R = ra["2012-12/2014-05"], Rf = rf["2012-12/2014-05"])
chart.CumReturns(position_return["2016-06/",1:2], legend.loc = "topleft", main = "From 2016-06 to now")
capm_table <- table.CAPM(Ra = ra["2016-06/"], Rb = rb["2016-06/"], Rf = rf["2016-06/"]) 
SharpeRatio.annualized(R = ra["2016-06/"], Rf = rf["2016-06/"])
chart.CumReturns(ra)
chart.CumReturns(rb)

return_df = position_return
start_date = "2008-01"
end_date = "2009-02"
  test_period = paste(start_date, "/", end_date, sep = "")
  chart.CumReturns(return_df[test_period,1:2], legend.loc = "topleft", main = paste("From", as.character(start_date), "to", as.character(end_date), sep = ""))
  SharpeRatio.annualized(ra[test_period,], Rf = rf[test_period,])
  InformationRatio(Ra = ra[test_period,], Rb = rb[test_period,])
  table.CAPM(Ra = ra[test_period,], Rb = rb[test_period,], Rf =rf[test_period,])
  
