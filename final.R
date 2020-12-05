# Title     : TODO
# Objective : TODO
# Created by: dlqkhang8
# Created on: 12/5/20

library(readxl)
wage_data <- read_excel('wage5.xlsx')

hist(wage_data$wage, main = "Density histogram of Wage of Individuals",
     ylab = "Proportion",
     xlab = "Wage",
     probability = TRUE)

lines(density(wage_data$wage), lwd = 2, col = "red")

hist(wage_data$hours, main = "Density histogram of Weekly Working Hours of Individuals",
     ylab = "Proportion",
     xlab = "Weekly working hours",
     probability = TRUE,
     ylim = c(0, 0.2))

lines(density(wage_data$hours), lwd = 2, col = "red")

hist(wage_data$IQ, main = "Density histogram of IQ of Individuals",
     ylab = "Proportion",
     xlab = "IQ score",
     probability = TRUE,
     ylim = c(0, 0.03))

lines(density(wage_data$IQ), lwd = 2, col = "red")

hist(wage_data$age, main = "Density histogram of Age of Individuals",
     ylab = "Proportion",
     xlab = "Age",
     probability = TRUE,
     xlim =c(25, 40))

lines(density(wage_data$age), lwd = 2, col = "red")

black_propotion <- round(sum(wage_data$black) / length(wage_data$black), 3)
black_label <- paste("Black: ", toString(black_propotion))
not_black_label <- paste("Not Black: ", toString(1 - black_propotion))
title_black <- paste("Propotion of Black people of the sample with n = ",
                     toString(length(wage_data$black)))
pie(table(wage_data$black),
    labels = c(not_black_label, black_label),
    main = title_black)


married_propotion <- round(sum(wage_data$married / length(wage_data$married)), 3)
married_label <- paste("Married: ", toString(married_propotion))
not_married_label <- paste("Not married: ", toString(1 - married_propotion))
title_married <- paste("Propotion of Married people of the sample with n = ",
                       toString(length(wage_data$married)))

pie(table(wage_data$married),
    labels = c(not_married_label, married_label),
    main = title_married)
