## select stocks 
# load packages
library(tidyquant)
library(rio)

# merge cgo/weekly data 
cgo <- import("./data/cgo.sas7bdat", encoding = "UTF-8")  # cgo data [raw data]
trd_week <- import("./data/trd_week.sas7bdat", encoding = "UTF-8") # weekly trade data [raw data]

trd_cgo <- left_join(trd_week, cgo , by = c("Stkcd", "Trdwnt")) %>%
  na.omit() %>% 
  filter(Markettype == "1"| Markettype == "4") %>% 
  mutate(Stkcd = paste(as.character(Stkcd), if_else(Markettype == 1, ".SH", ".SZ"), sep = "")) %>%
  mutate(group = substr(Clsdt,1,7)) # 试试周 substr(Opndt,1,7)

# select stock 
trd_last <- trd_cgo %>%
  group_by(Stkcd, group) %>%
  arrange(Clsdt) %>%
  filter(row_number() == n())

trd_selected <- trd_last %>%
  arrange(group, desc(cgo)) %>%
  group_by(group) %>%
  filter(row_number() <=30) # selected stock

# export and import data
export(trd_selected,"./data/trd_selected.feather")
trd_selected <- import("./data/trd_selected.feather")

# manipulate daily data
trd_dalyr <- import("./data/trd_dalyr.sas7bdat", encoding = "UTF-8") # daily trade data [raw data]
trd_dalyr <- trd_dalyr %>% 
  mutate(Trddt = as.Date.character(Trddt), Trddt1 = Trddt) 
trd_dalyr$Trddt1 <- trd_dalyr$Trddt1 %m-% months(1)
#mutate(Trddt1 = as.Date.character(Trddt), Trddt2 = as.Date.character(Trddt), Trddt = as.Date.character(Trddt))
#month(trd_dalyr$Trddt1) = month(trd_dalyr$Trddt) - 1
#month(trd_dalyr$Trddt2) = if_else(is.na(month(trd_dalyr$Trddt1)) == TRUE, month(trd_dalyr$Trddt - 10) - 1,  month(trd_dalyr$Trddt1))
trd_dalyr <- trd_dalyr %>%
  mutate(group = substr(as.character(Trddt1), 1, 7))%>%
  mutate(Stkcd = paste(as.character(Stkcd), if_else(Markettype == 1, ".SH", ".SZ"), sep = ""))

export(trd_dalyr, "./data/trd_dalyr.feather")
trd_dalyr <- import("./data/trd_dalyr.feather")

# slice data
p_mini <- full_position %>%
  select(Stkcd, group, cgo = cgo.x, weight, weight1, holding, holding1, hand, hand1, hand_dif)

#  select and daily data
trd_day_selected <- inner_join(trd_dalyr, p_mini, by = c("Stkcd", "group"))  # final data
trd_day_selected <- arrange(trd_day_selected, Trddt)

export(trd_day_selected, "./data/trd_day_selected.feather") 
trd_day_selected <- import("./data/trd_day_selected.feather") 

daily_position <- summarise(group_by(trd_day_selected, Trddt), money = sum(hand * Clsprc, na.rm = TRUE))
plot(daily_position$Trddt, daily_position$money)
temp <- tail(daily_position, 200)
plot(temp$Trddt, temp$money)
temp1 <- head(daily_position, 200)
plot(temp1$Trddt, temp1$money)
temp2 <- slice(daily_position, 500:1000)
temp2 %>% ggplot(aes(x = Trddt, y = money)) + geom_line()
export(daily_position, "./data/daily_position.feather")
daily_position <- import("./data/daily_position.feather")


###
temp <- summarise(group_by(trd_day_selected, group), number = n())



### 检验是不是配对成功

trd_day_selected_suma <- trd_day_selected %>%
  group_by(Stkcd, group) %>%
  summarize(mean = mean(n()))


temp <- trd_dalyr %>%
  filter(Stkcd == "600132.SH")

anti <- anti_join(trd_selected, trd_day_selected_suma, by =c("Stkcd", "group"))


