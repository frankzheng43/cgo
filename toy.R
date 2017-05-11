demo <- tibble::tibble(a = 1:100,
                       b = rep(1,100),
                       Turnover = runif(100))
turnover <- as.list(demo$Turnover)
prod <- as.list(cumprod(turnover))
t <- as.list(1:length(demo$Turnover))
k <- 10
n<- as.list(1:k)
prod <- n
f2 <- function(n){
  prod[n] <- cumprod(turnover)
}

f1 <- function(t){
  
}