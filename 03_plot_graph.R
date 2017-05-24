## plotting
temp$group <- as.yearmon(temp$group)
ggplot(data = temp, mapping = aes(x = group, y = end_holding))

temp1 %>% ggplot(aes(x = Trddt, y = return)) + geom_line()
temp1 %>% ggplot(aes(x = Trddt, y = money)) + geom_line()

plot.xts(temp2)

