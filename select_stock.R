## select stocks 
# load packages
library(tidyquant)
library(rio)

# merge cgo/weekly data 
cgo <- import("./data/cgo1.sas7bdat", encoding = "UTF-8") %>% # cgo data [raw data]
  select(Stkcd, Trdwnt, cgo) 

trd_week <- import("./data/trd_week.sas7bdat", encoding = "UTF-8") %>%# weekly trade data [raw data]
  select(-Capchgdt, -Ndaytrd, -Wretnd)

trd_cgo <- left_join(trd_week, cgo , by = c("Stkcd", "Trdwnt")) %>%
  na.omit() %>%
  filter(Markettype == "1"| Markettype == "4") %>% 
  mutate(Stkcd = paste(as.character(Stkcd), if_else(Markettype == 1, ".SH", ".SZ"), sep = "")) %>%
  mutate(group = substr(Clsdt,1,7), group_cgo = substr(as.character(ymd(Clsdt) %m+% months(1)), 1, 7)) %>%
  group_by(Stkcd, group) %>%
  arrange(Clsdt) %>%
  filter(row_number() == n()) %>%
  ungroup()
cgo_temp <- trd_cgo %>% select(Stkcd, cgo, group_cgo)
trd_cgo <- trd_cgo %>% rename(cgo1 = cgo) %>% select(-group_cgo) 
trd_cgo <- left_join(trd_cgo, cgo_temp, by = c("Stkcd", "group" = "group_cgo")) %>% 
  na.omit() %>%
  arrange(Stkcd)
remove(cgo_temp)
trd_selected <- trd_cgo %>%
  arrange(group, desc(cgo)) %>%
  group_by(group) %>%
  filter(row_number() <= 30) # selected stock

# export and import data
export(trd_selected,"./data/trd_selected.feather")
trd_selected <- import("./data/trd_selected.feather")

# manipulate daily data
trd_dalyr <- import("./data/trd_dalyr.sas7bdat", encoding = "UTF-8") # daily trade data [raw data]
trd_dalyr <- trd_dalyr %>% 
  mutate(Trddt = as.Date.character(Trddt), Trddt1 = Trddt) 
trd_dalyr$Trddt1 <- trd_dalyr$Trddt1 %m-% months(1)

trd_dalyr <- trd_dalyr %>%
  mutate(group = substr(as.character(Trddt1), 1, 7))%>%
  mutate(Stkcd = paste(as.character(Stkcd), if_else(Markettype == 1, ".SH", ".SZ"), sep = ""))

export(trd_dalyr, "./data/trd_dalyr.feather")
trd_dalyr <- import("./data/trd_dalyr.feather")

# close price
cls_price <- trd_dalyr %>%
  select(Stkcd, Trddt, cls_price = Clsprc, Markettype, adj_price = Adjprcwd) %>%
  mutate(group = substr(as.character(Trddt), 1, 7))%>%
  group_by(Stkcd, group) %>%
  arrange(Trddt) %>%
  filter(row_number() == n()) %>%
  select(Stkcd, group, cls_price, adj_price)
export(cls_price, "./data/cls_price.feather")
# gen groups
position <- trd_selected %>%
  select(Stkcd, group, Wsmvosd, Clsdt) %>%
  group_by(group) %>%
  mutate(Clsdt = as.Date.character(Clsdt)) %>%
  mutate(group1 = substr(as.character(Clsdt %m-% months(1)), 1,7)) %>%
  ungroup()

# 

position_cls <- left_join(position, cls_price, by = c("Stkcd", "group")) 
cls_price <- cls_price 
position_cls <- left_join(position_cls, cls_price, by = c("Stkcd", "group1" = "group"))
position_cls <- position_cls %>%
  select(-group1) %>%
  group_by(group) %>%
  mutate(weight = Wsmvosd/sum(Wsmvosd, na.rm = TRUE)) 

position_cls[,c("weight")] <-
  apply(position_cls[,c("weight")], 2, function(x){replace(x, is.na(x), 0)}) 

# 
p_cls_split <- split(position_cls, position_cls$group)
ini <- 1000000
fee <- 0.0002
k <- 100
p_cls_split[[k]]$start_holding = ini
p_cls_split[[k]]$start_hd = p_cls_split[[k]]$start_holding * p_cls_split[[k]]$weight
p_cls_split[[k]]$end_hd = p_cls_split[[k]]$start_hd * p_cls_split[[k]]$adj_price.x / p_cls_split[[k]]$adj_price.y
p_cls_split[[k]]$end_holding = sum(p_cls_split[[k]]$end_hd, na.rm = TRUE)
p_cls_split[[k]]$hand = p_cls_split[[k]]$start_hd / p_cls_split[[k]]$cls_price.y

for (i in (k+1) : length(p_cls_split)) {
  p_cls_split[[i]]$start_holding <- as.numeric(p_cls_split[[i-1]][1, "end_holding"])
  p_cls_split[[i]]$start_hd = p_cls_split[[i]]$start_holding * p_cls_split[[i]]$weight
  p_cls_split[[i]]$end_hd = p_cls_split[[i]]$start_hd * p_cls_split[[i]]$adj_price.x / p_cls_split[[i]]$adj_price.y
  p_cls_split[[i]]$end_holding = sum(p_cls_split[[i]]$end_hd, na.rm = TRUE)
  p_cls_split[[i]]$hand = p_cls_split[[i]]$start_hd / p_cls_split[[i]]$cls_price.y
}

position_cls <- bind_rows(p_cls_split)

temp$group <- as.yearmon(temp$group)
ggplot(data = temp, mapping = aes(x = group, y = end_holding))
# slice data
p_mini <- position_cls %>%
  select(Stkcd, group, hand, cls_price.x, start_hd, end_hd, adj_price.x, adj_price.y)

#  select and daily data
trd_day_selected <- inner_join(trd_dalyr, p_mini, by = c("Stkcd", "group"))  # final data
trd_day_selected <- arrange(trd_day_selected, Trddt)

export(trd_day_selected, "./data/trd_day_selected.feather") 
trd_day_selected <- import("./data/trd_day_selected.feather") 

daily_position <- summarise(group_by(trd_day_selected, Trddt), money = sum(start_hd * Adjprcwd / adj_price.y , na.rm = TRUE)) %>% na.omit() #
daily_position$return <- (daily_position$money - lag(daily_position$money))/ lag(daily_position$money)
plot(daily_position$Trddt, daily_position$return)
temp <- tail(daily_position, 200)
plot(temp$Trddt, temp$money)
temp1 <- head(daily_position, 200)
plot(temp1$Trddt, temp1$money)
temp2 <- slice(daily_position, 3000:5000)
temp2 %>% ggplot(aes(x = Trddt, y = return)) + geom_line()
temp2 %>% ggplot(aes(x = Trddt, y = money)) + geom_line()
export(daily_position, "./data/daily_position.feather")
daily_position <- import("./data/daily_position.feather")

temp1 <- daily_position %>% filter(!is.nan(return)) %>% na.omit() %>% filter( return != Inf)
temp2 <- as.xts(temp1$return, temp1$Trddt)
SharpeRatio(R = temp2, Rf = 0, FUN = "StdDev")
SharpeRatio(R = temp2["2010-01-01/2013-01-01"], Rf = 0, FUN = "StdDev")
temp2[temp2 > 1]
a <- trd_day_selected[trd_day_selected$Trddt == "2017-04-07",]
b <- trd_day_selected[trd_day_selected$Trddt == "2017-04-10",] #abnormal
###
temp <- summarise(group_by(trd_day_selected, group), number = n())



### 检验是不是配对成功

trd_day_selected_suma <- trd_day_selected %>%
  group_by(Stkcd, group) %>%
  summarize(mean = mean(n()))


temp <- trd_dalyr %>%
  filter(Stkcd == "600132.SH")

anti <- anti_join(trd_selected, trd_day_selected_suma, by =c("Stkcd", "group"))


