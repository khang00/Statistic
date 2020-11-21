pnorm(70, 68, 22)

data <- read.csv("pizza4.csv")
table(data$female)
table(data$hs)
attach(data)
barplot(table(data$hs))
table(female, grad)
barplot(table(data$female, data$grad), ylim = c(0, 25), beside = TRUE)
hist(data$pizza)
boxplot(pizza)
plot(income, pizza)

dbinom(5, size = 20, prob = 0.4)
pbinom(20, size = 20, prob = 0.4) - pbinom(9, size = 20, prob = 0.4, lower.tail = TRUE)
pbinom(15, size = 20, prob = 0.4)
pbinom(8, size = 20, prob = 0.4) - pbinom(3, size = 20, prob = 0.4, lower.tail = TRUE)

pnorm(5389.761, mean = 5000, sd = 100) - pnorm(5300, mean = 5000, sd = 100)
pnorm(4800, mean = 5000, sd = 100)
pnorm(5250, mean = 5000, sd = 100) - pnorm(4600, mean = 5000, sd = 100)

max(rnorm(40000, mean = 5000, sd = 100))
hist(rnorm(40000, mean = 5000, sd = 100))


