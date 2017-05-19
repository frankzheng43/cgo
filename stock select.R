# load packages
library(tidyquant)
library(rio)

# manipulate daily data
trd_dalyr <- import("./data/trd_dalyr.sas7bdat", encoding = "UTF-8") # daily trade data
trd_dalyr <- trd_dalyr %>% 
  mutate(Trddt = as.Date.character(Trddt)) %>%
  mutate(group = substr(as.character(Trddt-30), 1,7))%>%
  mutate(Stkcd = paste(as.character(Stkcd), if_else(Markettype == 1, ".SH", ".SZ"), sep = ""))

# merge cgo/ weekly and select stock 
cgo <- import("./data/cgo.sas7bdat", encoding = "UTF-8") %>% # cgo data
mutate(Stkcd = paste(as.character(Stkcd), if_else(Markettype == 1, ".SH", ".SZ"), sep = ""))
trd_week <- import("./data/trd_week.sas7bdat", encoding = "UTF-8") # weekly trade data 

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
  filter(row_number() <=30) # selected stock 

export(trd_selected,"./data/trd_selected.feather")
trd_selected <- import("./data/trd_selected.feather")

#  select and daily data
trd_day_selected <- inner_join(trd_dalyr, trd_selected, by = c("Stkcd", "group", "Markettype"))# final data

export(trd_day_selected, "./data/trd_day_selected.feather") 

trd_day_selected <- import("./data/trd_day_selected.feather") 

###
temp <- summarise(group_by(trd_day_selected, group), number = n())



### 检验是不是配对成功

trd_day_selected_suma <- trd_day_selected %>%
  group_by(Stkcd, group) %>%
  summarize(mean = mean(n()))


temp <- trd_dalyr %>%
  filter(Stkcd == "600132.SH")

anti <- anti_join(trd_selected, trd_day_selected_suma, by =c("Stkcd", "group"))


