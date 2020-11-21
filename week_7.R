# 9.25
sexual_harassment <- matrix(c(140, 192, 671, 106, 125, 732), ncol = 3, byrow = TRUE)
colnames(sexual_harassment) <- c("never", "once", "more than once")
rownames(sexual_harassment) <- c("girls", "boys")
sexual_harassment <- as.table(sexual_harassment)

# Compute count table
margin.table(sexual_harassment)
margin.table(sexual_harassment, 1)
margin.table(sexual_harassment, 2)

# Find the expected count
XsqSexual <- chisq.test(sexual_harassment)
XsqSexual$expected

# 9.26, the p value is smaller than 0.05, indicate that there is an dependency.
XsqSexual

# 9.31
dice <- matrix(c(89, 82, 123, 115, 100, 91), ncol = 6, byrow = TRUE)
colnames(dice) <- c(1, 2, 3, 4, 5, 6)
rownames(dice) <- "count"
dice <- as.table(dice)

XsqDice <- chisq.test(dice)
XsqDice$expected

# 9.32, the p value is smaller than 0.05, indicate that the outcome is not equally likely
XsqDice

# 9.37
fraud <- matrix(rbind(c(51, 12, 4), c(6, 5, 1)), ncol = 2, byrow = TRUE)
colnames(fraud) <- c("numbser allowed", "number not allowed")
rownames(fraud) <- c("small", "medium", "large")
fraud <- as.table(fraud)

# table of counts
margin.table(fraud)
margin.table(fraud, 1)
margin.table(fraud, 2)

# percent of claims not allowed
fraudProp <- prop.table(fraud, 1)
as.data.frame.matrix(fraudProp)$`number not allowed`

# combine medium and large, the medium and large have low propotion so we must combine these 2.
fraud[2,] <- fraud[2,] + fraud[3,]
fraud <- fraud[-3,]

# Ho:
# Ha:
# p value is large than 0.05 retain Ho.
XsqFraud <- chisq.test(fraud)
XsqFraud
