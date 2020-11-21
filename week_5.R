mean_pros_dist <- function(probalities, values, n) {
  mean_of_probalities_distribution <- 0
  for (i in 1:n) {
    mean_of_probalities_distribution <- mean_of_probalities_distribution + (probalities[i] * values[i])
  }
  mean_of_probalities_distribution
}

sd_pros_dist <- function(probalities, values, n) {

  mean_pros_dist <- function(probalities, values) {
    mean_of_probalities_distribution <- 0
    for (i in 1:n) {
      mean_of_probalities_distribution <- mean_of_probalities_distribution + (probalities[i] * values[i])
    }
    mean_of_probalities_distribution
  }

  mean <- mean_pros_dist(probalities, values)
  sd_of_probalities_distribution <- 0
  for (i in 1:n) {
    sd_of_probalities_distribution <- sd_of_probalities_distribution +
      probalities[i] *
        (mean - values[i])^2
  }
  sd_of_probalities_distribution <- sqrt(sd_of_probalities_distribution)
  sd_of_probalities_distribution
}

#===================================== 5.24 =====================================
math_grade_probalities <- c(0.33, 0.33, 0.2, 0.12, 0.02)
math_grades <- c(4, 3, 2, 1, 0)

# a.
mean_pros_dist(math_grade_probalities, math_grades, 5)
sd_pros_dist(math_grade_probalities, math_grades, 5)

# b.
math_sample <- 25
mean_of_means <- mean_pros_dist(math_grade_probalities, math_grades, 5)
sd_of_means <- sd_pros_dist(math_grade_probalities, math_grades, 5) / sqrt(math_sample)
mean_of_means
sd_of_means

# c.
0.33 + 0.33

# d.
1 - pnorm(3, mean = mean_of_means, sd = sd_of_means)

#===================================== 5.26 =====================================
payoff_probalities <- c(0.947, 0.026)
payoff <- c(0, 35)

# a.
mean_pros_dist(payoff_probalities, payoff, 2)
sd_pros_dist(payoff_probalities, payoff, 2)

# c.
casino_sample <- 265
mean_of_means <- mean_pros_dist(payoff_probalities, payoff, 2)
sd_of_means <- sd_pros_dist(payoff_probalities, payoff, 2) / sqrt(casino_sample)
mean_of_means
sd_of_means

# d.
hist(rnorm(casino_sample, mean_of_means, sd_of_means))
1 - pnorm(1, mean = mean_of_means, sd = sd_of_means, lower.tail = TRUE)

#===================================== 5.77 =====================================
# b.
1 - pnorm(124, mean = 123, sd = 0.08)

#===================================== 5.78 =====================================
cricket_mean <- 2.13
cricket_sd <- 1.88

# a.
cricket_rounds <- 140
mean_of_means <- cricket_mean
sd_of_means <- cricket_sd / sqrt(cricket_rounds)
mean_of_means
sd_of_means

# b.
pnorm(2, mean = mean_of_means, sd = sd_of_means, lower.tail = TRUE)

#===================================== 6.15 =====================================
alumni_sample_size <- 500
alumni_sample_mean <- 8.6
sd_pop <- 2.2

