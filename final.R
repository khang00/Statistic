# Title     : TODO
# Objective : TODO
# Created by: dlqkhang8
# Created on: 12/5/20

library(readxl)
wage_data <- read_excel('wage1.xls')

wage_summary <- summary(wage_data)
wage_sd <- sd(wage_data$wage)
IQ_sd <- sd(wage_data$IQ)
KWW_sd <- sd(wage_data$KWW)
age_sd <- sd(wage_data$age)
black_sd <- sd(wage_data$black)
south_sd <- sd(wage_data$south)
urban_sd <- sd(wage_data$urban)

wage_summary <- rbind(wage_summary, c(
  paste("Sd Var", toString(round(wage_sd, digits = 3)), sep = " :"),
  paste("Sd Var", toString(round(IQ_sd, digits = 3)), sep = " :"),
  paste("Sd Var", toString(round(KWW_sd, digits = 3)), sep = " :"),
  paste("Sd Var", toString(round(age_sd, digits = 3)), sep = " :"),
  paste("Sd Var", toString(round(black_sd, digits = 3)), sep = " :"),
  paste("Sd Var", toString(round(south_sd, digits = 3)), sep = " :"),
  paste("Sd Var", toString(round(urban_sd, digits = 3)), sep = " :")))
wage_summary

hist(wage_data$wage, main = "Density histogram of Wage of Individuals",
     ylab = "Proportion",
     xlab = "Wage",
     probability = TRUE)

lines(density(wage_data$wage), lwd = 2, col = "red")

boxplot(wage_data$wage)

hist(wage_data$IQ, main = "Density histogram of IQ of Individuals",
     ylab = "Proportion",
     xlab = "IQ score",
     probability = TRUE,
     ylim = c(0, 0.03))

lines(density(wage_data$IQ), lwd = 2, col = "red")

boxplot(wage_data$IQ)

hist(wage_data$KWW, main = "Density histogram of KWW of Individuals",
     ylab = "Proportion",
     xlab = "KWW",
     probability = TRUE,
     ylim = c(0, 0.06))

lines(density(wage_data$KWW), lwd = 2, col = "red")

boxplot(wage_data$KWW)

hist(wage_data$age, main = "Density histogram of Age of Individuals",
     ylab = "Proportion",
     xlab = "Age",
     probability = TRUE,
     xlim = c(25, 40))

lines(density(wage_data$age), lwd = 2, col = "red")

boxplot(wage_data$age)

black_propotion <- round(sum(wage_data$black) / length(wage_data$black), 3)
black_label <- paste("Black: ", toString(black_propotion))
not_black_label <- paste("Not Black: ", toString(1 - black_propotion))
title_black <- paste("Propotion of Black people of the sample with n = ",
                     toString(length(wage_data$black)))
pie(table(wage_data$black),
    labels = c(not_black_label, black_label),
    main = title_black)

south_propotion <- round(sum(wage_data$south) / length(wage_data$south), 3)
south_label <- paste("Southerner: ", toString(south_propotion))
not_south_label <- paste("Not Southerner: ", toString(1 - south_propotion))
title_south <- paste("Proporion of the Southerner of the sample with n = ", toString(length(wage_data$south)))

pie(table(wage_data$south),
    labels = c(not_south_label, south_label),
    main = title_south)

urban_propotion <- round(sum(wage_data$urban) / length(wage_data$urban), 3)
urban_label <- paste("Urban Inhabitant: ", toString(urban_propotion))
not_urban_label <- paste("Not Urban inhabitant: ", toString(1 - urban_propotion))
title_urban <- paste("Proporion of Ihe urban inhabitants of the sample with n = ", toString(length(wage_data$urban)))

pie(table(wage_data$urban),
    labels = c(not_urban_label, urban_label),
    main = title_urban)

plot(wage_data$wage, wage_data$urban)

t.test(wage_data$wage, mu = 900)
t.test(wage_data$IQ, mu = 100)
t.test(wage_data$KWW, mu = 38)
t.test(wage_data$age, mu = 32)

cor.test(wage_data$wage, wage_data$IQ)
cor.test(wage_data$wage, wage_data$KWW)
cor.test(wage_data$wage, wage_data$age)

cor.test(wage_data$wage, wage_data$black)
cor.test(wage_data$wage, wage_data$south)
cor.test(wage_data$wage, wage_data$urban)

library(purrr)

training_data <- head(wage_data, round(0.8 * length(wage_data$wage)))
test_data <- tail(wage_data, length(wage_data) - round(0.8 * length(wage_data$wage)))

discard_outliers <- function(wage_data) {
  wage_outlier <- boxplot(wage_data$wage, plot = FALSE)$out
  iq_outliers <- boxplot(wage_data$IQ, plot = FALSE)$out
  kkw_outlier <- boxplot(wage_data$KWW, plot = FALSE)$out

  filtered_wage <- wage_data %>%
    pmap(function(wage, IQ, KWW, age, black, south, urban) {
      c(wage, IQ, KWW, age, black, south, urban)
    }) %>%
    discard(function(row) { has_element(wage_outlier, row[1]) }) %>%
    discard(function(row) { has_element(iq_outliers, row[2]) }) %>%
    discard(function(row) { has_element(kkw_outlier, row[3]) })

  filtered_wage <- t(as.data.frame(filtered_wage))
  filtered_wage <- as.data.frame(filtered_wage)
  colnames(filtered_wage) <- c("wage", "IQ", "KWW", "age", "black", "south", "urban")
  rownames(filtered_wage) <- seq(from = 1, to = length(filtered_wage$wage))
  filtered_wage
}

filtered_wage <- discard_outliers(wage_data)
filtered_wage <- discard_outliers(filtered_wage)
filtered_wage <- discard_outliers(filtered_wage)
filtered_wage <- discard_outliers(filtered_wage)

wage_model <- lm(wage ~ KWW + IQ + age, data = filtered_wage)
qqnorm(wage_model$residuals)
qqline(wage_model$residuals)
shapiro.test(wage_model$residuals)
boxplot(wage_model$residuals)
model_summary <- summary(wage_model)

predicted_data <- predict(wage_model, wage_data)
actuals_preds <- data.frame(cbind(actuals = wage_data$wage, predicteds = predicted_data))
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals)) / actuals_preds$actuals)

layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional 4 graphs/page
plot(wage_model)
boxplot((wage_data$wage - predicted_data), plot = FALSE)$out
wage_model$coefficients
acf(residuals(wage_model))
plot(residuals(wage_model), main = "Residuals Plot",
     ylab = "Residuals")

plot(fitted(wage_model), residuals(wage_model), , main = "Residuals vs Fitteds Plot",
     ylab = "Residuals",
     xlab = "Fitteds")

plot(filtered_wage$wage, residuals(wage_model), main = "Residuals vs Wage Plot",
     ylab = "Wage",
     xlab = "Residual")

plot(filtered_wage$wage, fitted(wage_model))
abline(wage_model)
mean(residuals(wage_model))

cor.test(residuals(wage_model), filtered_wage$IQ)
cor.test(residuals(wage_model), filtered_wage$KWW)
cor.test(residuals(wage_model), filtered_wage$age)
