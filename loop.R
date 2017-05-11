
cgo1 <- function(V, P, k) {
  prodc <- 0
  prodd <- 0
  for (m in 1:k) {
    prodsum <- 1
    if (m == 1) {
      prodsum <- 1
    } else {
      for (ti in 1:(m - 1)) {
        prodsum <- (1 - lag(V, m - ti)[n - 1]) * prodsum
        print(prodsum)
      }
    }
    prodsumc <- lag(V, m) * lag(P, m) * prodsum
    prodsumd <- lag(V, m) * prodsum
    prodc <- prodsumc + prodc
    prodd <- prodsumd + prodd
  }
  RP <- prodc/prodd
  CGO <- (lag(P) - RP)/lag(P)
}
# cgo(0.2, 10, 3)
demo$cgo2 <- cgo(V = 1:100, P = 1:100, k = 10)
demo$cgo3 <- cgo(V = demo$Turnover, P = demo$Wclsprc, k = 10)
another <- trd_week %>% filter(Stkcd == "600031.SH")