## diagnose output data
# check if join works
trd_day_selected_suma <- trd_day_selected %>%
  group_by(Stkcd, group) %>%
  summarize(mean = mean(n()))
anti <- anti_join(trd_selected, trd_day_selected_suma, by =c("Stkcd", "group"))

# check which day did the return exceed 10%
daily_position[daily_position$return > 0.1, ]
daily_position[daily_position$return < -0.1, ]
#return SH300_return BIWI_return   CSI_return rf_rate
#2006-05-08 0.1181122   0.03932112          NA           NA 6.1e-05
#2006-06-01 0.1389829   0.02741072          NA           NA 6.1e-05
#2010-05-04 0.1052601  -0.01562123          NA -0.004070556 6.1e-05
position_return["2006-04-26/2006-05-09"]

hh <- trd_day_selected[trd_day_selected$Trddt == "2006-04-26" |trd_day_selected$Trddt == "2006-04-27" |trd_day_selected$Trddt == "2006-04-28"| trd_day_selected$Trddt == "2006-05-08" |trd_day_selected$Trddt == "2006-05-09",]
hh <- hh %>% arrange(Trddt, Stkcd)
hhh <- position_cls %>% filter(group == "2006-03"| group == "2006-04"| group == "2006-05"| group == "2006-06") %>% arrange(group, Stkcd)
# how many days does one stock trade in a month
temp <- summarise(group_by(trd_day_selected, group, Stkcd), number = n())
temp[temp$number < 10,]

# trace specific trade day when that day's holding is abnormal
b <- trd_day_selected[trd_day_selected$Trddt == "2017-04-10",] 
a <- trd_day_selected[trd_day_selected$Trddt == "2017-04-07",] 
dim(a) # expecting 30 rows
# how many days does our strategy outperform index
dim(position_index[position_index$return > position_index$SH300_return,])