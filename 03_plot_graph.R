## plotting
temp$group <- as.yearmon(temp$group)
ggplot(data = temp, mapping = aes(x = group, y = end_holding))

temp1 %>% ggplot(aes(x = Trddt, y = return)) + geom_line()
daily_position %>% ggplot(aes(x = Trddt, y = money)) + geom_line()

plot.xts(temp2)
chart.CumReturns(position_return, legend.loc = "topleft")
chart.CumReturns(position_return["2015/2016"])
chart.CumReturns(position_return[,1:2])
chart.CumReturns(ra)
chart.CumReturns(rb)