# Title     : TODO
# Objective : TODO
# Created by: dlqkhang8
# Created on: 10/7/20

library("readxl")
algorithm_type <- 6

IQR.outliers <- function(x) {
  if (any(is.na(x)))
    stop("x is missing values")
  if (!is.numeric(x))
    stop("x is not numeric")
  Q3 <- quantile(x, 0.75, type = algorithm_type)
  Q1 <- quantile(x, 0.25, type = algorithm_type)
  IQR <- (Q3 - Q1)
  left <- (Q1 - (1.5 * IQR))
  right <- (Q3 + (1.5 * IQR))
  c(x[x < left], x[x > right])
}

# Smolts
data <- read_excel("DATA WEEK 3/DATA WEEK 3/smolts.xlsx")
quantile(data$Percent, type = algorithm_type)
mean(data$Percent)
IQR(data$Percent, type = algorithm_type)
IQR.outliers(data$Percent)
boxplot(data, ylab = "Percentage")
boxplot.stats(data$Percent)$out