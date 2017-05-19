newcgo <- function(df, k) {
  V <- df$Turnover
  Vl <- 1-V
  P <- df$Wclsprc
}




cgo2 <- function(V, P, k, id) {
  prodc <- 0
  prodd <- 0
  for (m in 1:k) {
    prodsum <- 1
    if (m == 1) {
      prodsum <- 1
    } else {
      for (ti in 1:(m - 1)) {
        prodsum <- (1 - lag(V, m - ti)[[id - 1]]) * prodsum
        print(prodsum)
      }
    }
    prodsumc <- lag(V, m)[[id - 1]] * lag(P, m)[[id - 1]] * prodsum
    prodsumd <- lag(V, m)[[id - 1]]* prodsum 
    prodc <- prodsumc + prodc
    prodd <- prodsumd + prodd
  }
  RP <- prodc/prodd
  CGO <- (lag(P) - RP)/lag(P)
}