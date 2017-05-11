library(tidyquant)
library(readr)
trd_day<- read_csv("./data/trd_day.csv")
trd_week<-read_csv("./data/trd_week.csv")
demo<-head(trd_week, 1500)%>%
  group_by(Stkcd)%>%
  mutate(id=1:n())
demo <- write_csv(demo, "./data/demo.csv")