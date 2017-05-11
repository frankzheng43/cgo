library(tidyquant)
library(rio)
trd_dalyr <- import("./data/trd_dalyr.sas7bdat", encoding = "UTF-8")
cgo <- import("./data/cgo.sas7bdat", encoding = "UTF-8")
trd_week <- import("./data/trd_week.csv")

trd_cgo <- left_join(trd_week, cgo , by = c("Stkcd", "Trdwnt")) %>%
  na.omit() %>% 
  filter(Markettype == c(1, 4)) %>% 
  mutate(Stkcd = paste(as.character(Stkcd), if_else(Markettype == 1, ".SH", ".SZ"), sep = "")) %>%
  mutate(group = substr(Opndt,1,7))

trd_last <- trd_cgo %>%
  group_by(Stkcd, group) %>%
  filter(row_number() == n())

trd_selected <- trd_last %>%
  arrange(group, desc(cgo)) %>%
  group_by(group) %>%
  filter(row_number() <=30)


export(trd_selected,"./data/trd_selected.feather")
export(trd_day,"./data/trd_day.feather")

trd_selected <- import("./data/trd_selected.feather")
trd_day <- import("./data/trd_day.feather") 

trd_dalyr <- trd_dalyr %>% 
  mutate(Trddt = as.Date.character(Trddt)) %>%
  mutate(group = substr(as.character(Trddt-30), 1,7))%>%
  mutate(Stkcd = paste(as.character(Stkcd), if_else(Markettype == 1, ".SH", ".SZ"), sep = ""))
## 导出经过处理的日交易数据
export(trd_dalyr,"./data/trd_dalyr.feather")
trd_day_selected <- inner_join(trd_dalyr, trd_selected, by = c("Stkcd", "group", "Markettype"))
## 导出匹配后入选的股票的日交易数据
export(trd_day_selected, "./data/trd_day_selected.feather") 
export(trd_day_selected, "./data/trd_day_selected.csv") 
### 检验是不是配对成功

trd_day_selected_suma <- trd_day_selected %>%
  group_by(Stkcd, group) %>%
  summarize(mean = mean(n()))

temp <- trd_dalyr %>%
  filter(Stkcd == "600132.SH")

anti <- anti_join(trd_selected, trd_day_selected_suma, by =c("Stkcd", "group"))


