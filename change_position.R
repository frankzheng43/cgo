# 调仓
library(tidyquant)
library(rio)
trd_selected <- import("./data/trd_selected.feather")
position <- trd_selected %>%
  select(Stkcd, group, cgo,  Opndt, Clsdt, Wopnprc, Wclsprc, Wsmvosd) 
position <- position %>%
  group_by(group) %>%
  filter(n() >= 30 ) %>% #
  mutate(Opndt = as.Date.character(Opndt)) %>%
  mutate(group1 = substr(as.character(Opndt-30), 1,7)) %>%
  ungroup()
position1 <- position %>%
  select(-group1)
position2 <- position %>%
  select(-group)
ini = 2000000000
full_position <- full_join(position1, position2, by = c("Stkcd", c("group" = "group1"))) %>%
  group_by(group) %>%
  mutate(weight_x = Wsmvosd.x/sum(Wsmvosd.x, na.rm = TRUE), weight_y = Wsmvosd.y/sum(Wsmvosd.y, na.rm = TRUE)) 

full_position[,c("weight_y", "weight_x")] <-
  apply(full_position[,c("weight_x","weight_y")], 2, function(x){replace(x, is.na(x), 0)})
full_position <- full_position %>%
  group_by(group) %>%
  arrange(group) %>%
  mutate(holding_x = ini, 
         hand_x = holding_x * weight_x / Wopnprc.x, 
         holding_y = sum(hand_x * Wopnprc.y, na.rm = TRUE), 
         hand_y =holding_y * weight_y / Wopnprc.y) 
full_position1 <- full_position %>%
  group_by(group) %>%
  mutate(weight_x = Wsmvosd.x/sum(Wsmvosd.x, na.rm = TRUE), weight_y = Wsmvosd.y/sum(Wsmvosd.y, na.rm = TRUE))
  
temp <- summarise(group_by(position, group), number = n())


export(full_position, "./data/full_position.csv")