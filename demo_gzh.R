data <- demo$Turnover

f1 <- function(x){
  c(rep(NA,x),data[1:(length(data)-x)])
}
demolist <- as.list(1:256)
data_list <- lapply(demolist,f1)
data_list
vprod <- as.list(1: length(data))
f2 <- function(x) {
  vprod[ (x + 1)] <- rowProds(as.matrix(vllag[, 1:x]))
  return(vprod)
}
data_list <- lapply(demolist, f2)
vlag <- do.call(cbind.data.frame,data_list)
vllag <- do.call(cbind.data.frame,data_list)
names(vlag) <- c(paste0("lag",rep(1:256)))
