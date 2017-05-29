## plotting
# overall
chart.CumReturns(position_return, legend.loc = "topleft", main = "From 2004-05-11 to 2017-04-17")
# bull periods
chart.CumReturns(position_return["2006-01/2007-11",1:2], legend.loc = "topleft", main = "From 2006-01 to 2007-11")
chart.CumReturns(position_return["2014-04/2015-04",1:2], legend.loc = "topleft", main = "From 2014-04 to 2015-04")
# bear periods
chart.CumReturns(position_return["2008-01/2009-02",1:2], legend.loc = "topleft")
chart.CumReturns(position_return["2011-07/2012-11",1:2], legend.loc = "topleft")
chart.CumReturns(position_return["2015-07/2016-05",1:2], legend.loc = "topleft")
# uncertain periods
chart.CumReturns(position_return["2009-03/2011-06",1:2], legend.loc = "topleft")
chart.CumReturns(position_return["2012-12/2014-05",1:2], legend.loc = "topleft")
chart.CumReturns(position_return["2016-06/",1:2], legend.loc = "topleft")

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
  
  charts.PerformanceSummary(ra, rf)
  calendar_return <- table.CalendarReturns(ra)
  export(calendar_return, "./data/calendar_return.csv")
  drawdowns <- table.Drawdowns(ra)
  export(drawdowns,"./data/drawdowns.csv")
