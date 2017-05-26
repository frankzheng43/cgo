## index benchmark data
rf_rate <- import("./data/rf.csv") %>% mutate(Trddt = ymd(Trddt))
index <- import("./data/index.xlsx") %>% mutate(Trddt = ymd(Trddt))
position <- left_join(daily_position, index, by = "Trddt") %>%
  left_join(rf_rate, by = "Trddt") %>%
  rename(rf_rate = Rf) %>%
  mutate(SH300_return = SH300/lag(SH300) - 1, BIWI_return = BIWI/lag(BIWI) - 1, CSI_return = CSI/lag(CSI) - 1)
position_return <- position %>%
  select(Trddt, ends_with("return"),rf_rate) %>%
  as_xts(date_col = Trddt)
position_index <- position %>%
  select(-ends_with("return"),-rf_rate) %>%
  as_xts(date_col = Trddt)