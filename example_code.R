########################################################################
# example code for mdstdbscan                                          #
# Changlock Choi                                                       #
# 2021-05-25                                                           #
########################################################################

## measuring calculation time
time.table <- data.frame()

for(i in seq(50, 10000, by = 50)){
  x.cord <- c(rnorm(i))
  y.cord <- c(rnorm(i))
  time.cord <- c(rnorm(i))
  add.value <- c(rnorm(i))

  starttime <- Sys.time()
  ww1 <- mdstdbscan(x = x.cord, y = y.cord,
                    time = time.cord, value = add.value,
                    eps = 0.5, eps2 = 0.5, eps3 = 0.5, minpts = 10)
  endtime <- Sys.time()
  cal.time <- endtime - starttime
  time.table[i/50, 1] <- cal.time
}  

## verification for Chameleon data set
library(seriation)

data(Chameleon)

tchamel5 <- chameleon_ds5

## adding time variable to Chameleon data set
tchamel5[which(tchamel5$x < 170), 3] <- 1
tchamel5[which(tchamel5$x >= 170 & tchamel5$x < 270), 3] <- 2
tchamel5[which(tchamel5$x >= 270 & tchamel5$x < 420), 3] <- 3
tchamel5[which(tchamel5$x >= 420 & tchamel5$x < 550), 3] <- 4
tchamel5[which(tchamel5$x >= 550 & tchamel5$x < 680), 3] <- 5
tchamel5[which(tchamel5$x >= 680 & tchamel5$x < 900), 3] <- 6

plot(tchamel5[, 1:2], col = tchamel5$V3)

## adding additional variable to Chameleon data set
x.ord <- order(tchamel5$x)

tchamel5[x.ord[1:1000], 4] <- rnorm(1000, mean = 100, sd = 30)
tchamel5[x.ord[1001:2000], 4] <- rnorm(1000, mean = 200, sd = 30)
tchamel5[x.ord[2001:3000], 4] <- rnorm(1000, mean = 300, sd = 30)
tchamel5[x.ord[3001:4000], 4] <- rnorm(1000, mean = 400, sd = 30)
tchamel5[x.ord[4001:5000], 4] <- rnorm(1000, mean = 500, sd = 30)
tchamel5[x.ord[5001:6000], 4] <- rnorm(1000, mean = 600, sd = 30)
tchamel5[x.ord[6001:7000], 4] <- rnorm(1000, mean = 700, sd = 30)
tchamel5[x.ord[7001:8000], 4] <- rnorm(1000, mean = 800, sd = 30)

## making multidimensinal cluster with mdstdbscan
result <- mdstdbscan(x = tchamel5$x, y = tchamel5$y, time = tchamel5$V3,
                     value = tchamel5[, 4], eps = 10, eps2 = 0.5, eps3 = 10,
                     minpts = 8)

## simple visualization of result
plot(tchamel5[, 1:2], col = result$cluster)
