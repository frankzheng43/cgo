library(haven)
#trd_dalyr <- read_sas("F:/Rproject/CGO/data/trd_dalyr.sas7bdat", encoding = "UTF-8")
library(tidyquant)
library(lubridate)
need <- c("Stkcd", "Trddt", "Opnprc", "Hiprc", "Loprc", "Clsprc", "Dnshrtrd", 
          "Dsmvosd", "Adjprcwd", "Markettype")
trd <- trd_dalyr %>%
  select(1:7, 9, 13, 15) %>%
  filter(Markettype == c(1, 4)) %>% # 流通股数和换手率
  mutate(Dntrs = Dsmvosd/Adjprcwd, Turnover = Dnshrtrd/Dntrs) %>% 
  mutate(Stkcd = paste(as.character(Stkcd), if_else(Markettype == 1, ".SH", ".SZ"), sep = "")) %>%
  mutate(Trddt = ymd(Trddt))
# head(as_xts(trd,Trddt,c(3:6)))
write_csv(trd, "./data/trd_day.csv")

trd_week <- read_sas("F:/Rproject/CGO/data/trd_week.sas7bdat", encoding = "UTF-8")
demo <- trd_week %>% # select(1:7,9,13,15)%>%
  filter(Markettype == c(1, 4)) %>% # 流通股数和换手率
  mutate(Wntrs = Wsmvosd/Wclsprc, Turnover = Wnshrtrd/Wntrs) %>% 
  mutate(Stkcd = paste(as.character(Stkcd), if_else(Markettype == 1, ".SH", ".SZ"), sep = "")) %>%
  group_by(Stkcd) #%>%
  # 除1000使得换手率看起来正常
  #mutate(Turnover = Turnover /1000, Vl = 1- Turnover)
remove(trd_week)
# mutate(Trddt=ymd(Trddt))

#取一个小样本试验
demo <- head(trd_week,15000) %>%
  group_by(Stkcd) %>%
  # 除1000使得换手率看起来正常
  mutate(Turnover = Turnover /1000, Vl = 1- Turnover) %>%
  sample_n(10)
ni<- 265

vlag <- tibble(stock = demo$Stkcd, V0 = demo$Turnover) %>% 
  group_by(stock)

vllag <- tibble(stock = demo$Stkcd, V0 = demo$Vl) %>% 
  group_by(stock)

P <- tibble(stock = demo$Stkcd, P0 = demo$Wclsprc) %>% 
  group_by(stock)

vprod <- tibble(stock = demo$Stkcd, vprod0 = 1) %>% 
  group_by(stock)

a <- tibble(stock = demo$Stkcd)
b <- tibble(stock = demo$Stkcd)

k <- ni
for (i in 1:k) { 
  vllag[, (i + 2)] <- dplyr::lag(demo$Vl, i)
}
library(matrixStats)
remove(demo)
for (i in 2:k){
  vprod[, (i + 1)] <- rowProds(as.matrix(vllag[, 3:(i -1 + 2)]))
}
remove(vllag)
write_csv(vprod,"./data/vprod.csv")
for (i in 1:k){
  #vlag[, (i + 2)] <- dplyr::lag(demo$Turnover, i)
  #P[, (i + 2)] <- dplyr::lag(demo$Wclsprc, i)
  # vprod[, i] <- apply(vllag[,2:i], 1, prod, na.rm = TRUE)
  a[, (i + 1)] <- vprod[, (i + 1)] * vlag[, (i + 2)] * P[, (i + 2)]
  b[, (i + 1)] <- vprod[, (i + 1)] * vlag[, (i + 2)]
}
remove(demo)
for (i in 1:k){
  a$suma <- rowSums(a[,2:(k+1)])
  b$sumb <- rowSums(b[,2:(k+1)])
}
demo$RP <- a$suma/b$sumb 

demo <-demo %>%
  group_by(Stkcd) %>%
  mutate(cgo = (lag(Wclsprc,1) - RP) / lag(Wclsprc,1))




write_csv(demo, "./data/trd_week.csv")
trd_week <- read_csv("./data/trd_week.csv")
trd_week_om <- na.omit(trd_week)