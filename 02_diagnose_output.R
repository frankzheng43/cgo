## diagnose output data
# check if join works
trd_day_selected_suma <- trd_day_selected %>%
  group_by(Stkcd, group) %>%
  summarize(mean = mean(n()))
anti <- anti_join(trd_selected, trd_day_selected_suma, by =c("Stkcd", "group"))

# check which day did the return exceed 10%
temp2[temp2 > 0.1]

# how many days does one stock trade in a month
temp <- summarise(group_by(trd_day_selected, group, Stkcd), number = n())
temp[temp$number < 10,]

# trace specific trade day when that day's holding is abnormal
b <- trd_day_selected[trd_day_selected$Trddt == "2017-04-10",] 
a <- trd_day_selected[trd_day_selected$Trddt == "2017-04-07",] 
dim(a) # expecting 30 rows
