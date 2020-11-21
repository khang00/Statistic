  binom4 <- rbinom(1000, 10, 0.4)
hist(binom4, freq <- FALSE)
curve(dnorm(x, mean <- mean(binom4), sd <- sd(binom4)), add <- TRUE, col <- "red")

binom5 <- rbinom(1000, 10, 0.5)
hist(binom5, freq <- FALSE)
curve(dnorm(x, mean <- mean(binom5), sd <- sd(binom5)), add <- TRUE, col <- "red")

poisson <- rpois(1000, 3)
hist(poisson, freq <- FALSE)
curve(dnorm(x, mean <- mean(poisson), sd <- sd(poisson)), add <- TRUE, col <- "red")

normal <- rnorm(1000, 3, 4)
hist(normal, freq <- FALSE)
line(density(normmal))

qnorm(0.03, 3, 2) - qnorm(0.97, 3, 2)

library('Rlab')

r <- 1000
c <- 1000

data <- matrix(0, r, c) #simulation du lieu tu normal

for (i in 1:c) {
  data[, i] <- rnorm(r, 3, 2)
}

#a. Tim trung binh noi bo
mean_data <- c()
for (i in 1:c) {
  mean_data <- c(mean_data, mean(data[, i]))
}

mean_data_normal

#b. ve hist cua cac TB
hist(mean_data, probability <- TRUE)
lines(density(mean_data))

#c. Tim TB va do lech chuanb cua cac TB
mean_Mean <- mean(mean_data)
std_Mean <- sd(mean_data)
mean_Mean
std_Mean

###############
r <- 100
c <- 20

data <- matrix(0, r, c)

for (i in 1:c) {
  data[, i] <- rbinom(r, 1, 0.3)
}

#a. Tim trung binh noi bo
mean_data <- c()
lines <- c()
for (i in 1:c) {
  mean_data <- c(mean_data, mean(data[, i]))
}

#b. ve hist cua cac TB
hist(mean_data, probability = TRUE)
lines(density(mean_data))

#c. Tim TB va do lech chuanb cua cac TB
mean_Mean <- mean(mean_data)
std_Mean <- sd(mean_data)
mean_Mean
std_Mean

plot(c(1, 2))
segments(1, 1, 4, 4)

qnorm(0.7, 68, 0.98)
