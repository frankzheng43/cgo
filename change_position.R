position1 <- position %>%
  select(-group1)
position2 <- position %>%
  select(-group)
ini = 2000000 # initial position
fee = 0.0002 # left

full_position <- full_join(position1, position2, by = c("Stkcd", c("group" = "group1"))) %>%
  group_by(group) %>%
  arrange(group) %>%
  mutate(weight = Wsmvosd.x/sum(Wsmvosd.x, na.rm = TRUE), weight1 = Wsmvosd.y/sum(Wsmvosd.y, na.rm = TRUE) ) 
# remove NA
full_position[,c("weight", "weight1")] <-
  apply(full_position[,c("weight", "weight1")], 2, function(x){replace(x, is.na(x), 0)})
# split into list 
p_split <- split(full_position, full_position$group) 
# remove first element
p_split[1] <- NULL
# 
# initialize
p_split[[1]]$holding <- ini
p_split[[1]]$hand <- p_split[[1]]$holding * (1-fee) * p_split[[1]]$weight / p_split[[1]]$Wclsprc.x
# calculate holding and stock in hand
for (i in 2 : length(p_split)) {
  p_split[[i]]$holding <- sum(p_split[[i-1]]$hand * p_split[[i-1]]$Wclsprc.x, na.rm = TRUE) # 错了
  p_split[[i]]$hand <- p_split[[i]]$holding * (1-fee) * p_split[[i]]$weight / p_split[[i]]$Wclsprc.x
}
# calculate holding and stock in hand next month
for (i in 1 :(length(p_split) - 1)) {
  p_split[[i]]$holding1 <- as.numeric(p_split[[i+1]][1,"holding"])
  p_split[[i]]$hand1 <- p_split[[i]]$holding1 * p_split[[i]]$weight1 /  p_split[[i]]$Wopnprc.y
}
# last element
p_split[[length(p_split)]]$holding1 <- NA
p_split[[length(p_split)]]$hand1 <- NA
# combind list into data.frame
full_position <- bind_rows(p_split)
# remove NA
full_position[,c("hand", "hand1")] <-
  apply(full_position[,c("hand", "hand1")], 2, function(x){replace(x, is.na(x), 0)})
# calculate the dif of stock in hand 
full_position <- full_position %>%
  mutate(hand_dif = hand1-hand)


##
temp <- summarise(group_by(position, group), number = n())

export(full_position, "./data/full_position.csv")
export(full_position, "./data/full_position.feather")

full_position <- import("./data/full_position.feather")