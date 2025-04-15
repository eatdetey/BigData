time <- numeric(10)
RES <- numeric(10)
NUMB <- 1:10

start_time <- Sys.time()

xA <- seq(100, 200, by =5)
RES[1] <- sum(xA)
t1 <- Sys.time()
time[1] <- t1-start_time

RES[2] <- length(xA)
RES[3] <- mean(xA)
t2 <- Sys.time()
time[2]<- t2-t1

norm <- rnorm(length(xA)+7, 5)
norm_var <- var(norm)
RES[4] <- round(norm_var, 0)
t3 <- Sys.time()
time[3] <- t3-t2

arr <- array(xA, c(5,length(xA)/5))
sinsum <- sum(sin(arr))
RES[5] <- round(sinsum, 4)
t4 <- Sys.time()
time[4] <- t4-t3

xA <- append(xA, seq(205, 220, by = 5))
length(xA)
matr <- matrix(xA, nrow = 5)
matr <- matr[-2,]
matr <- matr[-5,]
matr
RES[6] <- nrow(matr)+ncol(matr)
t5 <- Sys.time()
time[5] <- t5-t4

new_list <-rep(c(TRUE, FALSE), 5)
new_list
new_list_sum <- any(new_list)
RES[7] <- new_list_sum
t6 <- Sys.time()
time[6] <- t6-t5

RES[8] <- identical(arr, matr)
t7 <- Sys.time()
time[7] <- t7-t6

arr
matr
matr <-array(matr, c(4,5))
arr <- arr[-2,]
arr <-arr[-5,]
arr
RES[9] <- identical(arr, matr)
t8 <- Sys.time()
time[8] <- t8-t7

result <- data.frame(
  TIME <- time,
  TASK <- NUMB,
  RESULTS <- RES
)
result
t9<- Sys.time()
t9-start_time
