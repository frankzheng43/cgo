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

# manipulate daily data
trd_dalyr <- import("./data/trd_dt5.sas7bdat", encoding = "UTF-8") # daily trade data [raw data]
trd_dalyr <- trd_dalyr %>% 
  mutate(Trddt = as.Date.character(Trddt), Trddt1 = Trddt) 

trd_dalyr$Trddt1 <- trd_dalyr$Trddt1 %m-% months(1)

trd_dalyr <- trd_dalyr %>%
  mutate(group = substr(as.character(Trddt1), 1, 7))%>%
  mutate(Stkcd = paste(as.character(Stkcd), if_else(Markettype == 1, ".SH", ".SZ"), sep = ""))

# close price
cls_price <- trd_dalyr %>%
  select(Stkcd, Trddt, cls_price = Clsprc, Markettype, adj_price = Adjprcwd) %>%
  mutate(group = substr(as.character(Trddt), 1, 7))%>%
  group_by(Stkcd, group) %>%
  arrange(Trddt) %>%
  filter(row_number() == n()) %>% # select the last day of every month
  select(Stkcd, group, cls_price, adj_price)

# gen position
position <- trd_selected %>%
  select(Stkcd, group, Wsmvosd, Clsdt) %>%
  group_by(group) %>%
  mutate(Clsdt = as.Date.character(Clsdt)) %>%
  mutate(group1 = substr(as.character(Clsdt %m-% months(1)), 1,7)) %>% # mismatch by one month in order to merge
  ungroup()

# position with close price
position_cls <- left_join(position, cls_price, by = c("Stkcd", "group")) 
position_cls <- left_join(position_cls, cls_price, by = c("Stkcd", "group1" = "group"))
position_cls <- position_cls %>%
  select(-group1) %>%
  group_by(group) %>%
  mutate(weight = Wsmvosd/sum(Wsmvosd, na.rm = TRUE)) 

position_cls[,c("weight")] <-
  apply(position_cls[,c("weight")], 2, function(x){replace(x, is.na(x), 0)})  # remove NA (optinal)

# monthly holding and hand
# split full dataframe into lists and apply 
p_cls_split <- split(position_cls, position_cls$group)

# initial position
ini <- 1000000
fee <- 0.0002
k <- 100 # which month to start
p_cls_split[[k]]$start_holding = ini
p_cls_split[[k]]$start_hd = p_cls_split[[k]]$start_holding * p_cls_split[[k]]$weight
p_cls_split[[k]]$end_hd = p_cls_split[[k]]$start_hd * p_cls_split[[k]]$adj_price.x / p_cls_split[[k]]$adj_price.y
p_cls_split[[k]]$end_holding = sum(p_cls_split[[k]]$end_hd, na.rm = TRUE)
p_cls_split[[k]]$hand = p_cls_split[[k]]$start_hd / p_cls_split[[k]]$cls_price.y

# loop over (stupid way)
for (i in (k+1) : length(p_cls_split)) {
  p_cls_split[[i]]$start_holding <- as.numeric(p_cls_split[[i-1]][1, "end_holding"])
  p_cls_split[[i]]$start_hd = p_cls_split[[i]]$start_holding * p_cls_split[[i]]$weight
  p_cls_split[[i]]$end_hd = p_cls_split[[i]]$start_hd * p_cls_split[[i]]$adj_price.x / p_cls_split[[i]]$adj_price.y
  p_cls_split[[i]]$end_holding = sum(p_cls_split[[i]]$end_hd, na.rm = TRUE)
  p_cls_split[[i]]$hand = p_cls_split[[i]]$start_hd / p_cls_split[[i]]$cls_price.y
}

# rebind list into dataframe
position_cls <- bind_rows(p_cls_split)

# part of the full data
p_mini <- position_cls %>%
  select(Stkcd, group, hand, cls_price.x, start_hd, end_hd, adj_price.x, adj_price.y)

#  select and daily data
trd_day_selected <- inner_join(trd_dalyr, p_mini, by = c("Stkcd", "group"))  # final data
trd_day_selected <- arrange(trd_day_selected, Trddt)

# summarise into one
daily_position <- summarise(group_by(trd_day_selected, Trddt), money = sum(start_hd * Adjprcwd / adj_price.y , na.rm = TRUE)) %>% na.omit() 
daily_position$return <- (daily_position$money - lag(daily_position$money))/ lag(daily_position$money) # final data for plotting
temp1 <- daily_position %>% filter(!is.nan(return)) %>% na.omit() %>% filter( return != Inf)
temp2 <- as.xts(temp1$return, temp1$Trddt)

# export and import data
export(trd_selected,"./data/trd_selected.feather")
trd_selected <- import("./data/trd_selected.feather")
export(trd_dalyr, "./data/trd_dalyr.feather")
trd_dalyr <- import("./data/trd_dalyr.feather")
export(daily_position, "./data/daily_position.feather")
daily_position <- import("./data/daily_position.feather")
