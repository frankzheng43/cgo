demo <- trd_week %>%
  filter(Markettype == c(1, 4)) %>% 
  # 流通股数和换手率
  mutate(Wntrs = Wsmvosd/Wclsprc, Turnover = Wnshrtrd/Wntrs) %>% 
  mutate(Stkcd = paste(as.character(Stkcd), if_else(Markettype == 1, ".SH", ".SZ"), sep = "")) %>%
  group_by(Stkcd) %>%
  # 除1000使得换手率看起来正常
  mutate(Turnover = Turnover /1000, Vl = 1- Turnover)

remove(trd_week) #清理内存

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

remove(demo)#清理内存
for (i in 2:k){
  vprod[, (i + 1)] <- rowProds(as.matrix(vllag[, 3:(i -1 + 2)]))#这里会算很久很久
}
remove(vllag)#清理内存

for (i in 1:k){
  a[, (i + 1)] <- vprod[, (i + 1)] * vlag[, (i + 2)] * P[, (i + 2)]
  b[, (i + 1)] <- vprod[, (i + 1)] * vlag[, (i + 2)]
}
remove(vlag)#清理内存
remove(vprod)#清理内存

#算到这里算不动了
for (i in 1:k){
  a$suma <- rowSums(a[,2:(k+1)])
  b$sumb <- rowSums(b[,2:(k+1)])
}
trd_week <- read_sas("F:/Rproject/CGO/data/trd_week.sas7bdat", encoding = "UTF-8") #数据的位置需要更改
#重新载入数据
demo <- trd_week %>% # select(1:7,9,13,15)%>%
  filter(Markettype == c(1, 4)) %>% 
  # 流通股数和换手率
  mutate(Wntrs = Wsmvosd/Wclsprc, Turnover = Wnshrtrd/Wntrs) %>% 
  mutate(Stkcd = paste(as.character(Stkcd), if_else(Markettype == 1, ".SH", ".SZ"), sep = "")) %>%
  group_by(Stkcd) %>%
  # 除1000使得换手率看起来正常
  mutate(Turnover = Turnover /1000, Vl = 1- Turnover)
demo$RP <- a$suma/b$sumb 

demo <-demo %>%
  group_by(Stkcd) %>%
  mutate(cgo = (lag(Wclsprc,1) - RP) / lag(Wclsprc,1))