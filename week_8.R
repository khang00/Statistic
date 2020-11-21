# ================================ 9.25 =====================================================
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

# ================================ 9.26 =====================================================
# the p value is smaller than 0.05, indicate that there is an dependency.
XsqSexual

# 9.31
dice <- matrix(c(89, 82, 123, 115, 100, 91), ncol = 6, byrow = TRUE)
colnames(dice) <- c(1, 2, 3, 4, 5, 6)
rownames(dice) <- "count"
dice <- as.table(dice)

XsqDice <- chisq.test(dice)
XsqDice$expected

# ================================ 9.32 =====================================================
# the p value is smaller than 0.05, indicate that the outcome is not equally likely
XsqDice

# ================================ 9.37 =====================================================
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

# Ho: there is no dependency between the size of claims and the acceptence of the claims
# Ha: there is an dependency between the size of claims and the acceptence of the claims
# p value is large than 0.05 retain Ho.
XsqFraud <- chisq.test(fraud)
XsqFraud

# ================================ 9.43 =====================================================
# compute counts table for funding of student
numberOfStundent <- c(942, 599, 5234, 3238, 1378, 2300)
propAccquaintance <- c(0.2, 0.37, 0.26, 0.16, 0.18, 0.41)
fund <- matrix(rbind(round(propAccquaintance * numberOfStundent),
                     round(numberOfStundent - (propAccquaintance * numberOfStundent))),
               ncol = 2, byrow = TRUE)

colnames(fund) <- c("parents/family/spouse", "other")
rownames(fund) <- c("trades", "design", "health", "media/IT", "service", "other")
fund <- as.table(fund)

# Ho: There is no association between fields of study and funding source from parents/family/spouse
# Ha: There is an association between fields of study and funding source from parents/family/spouse
# p value is smaller than 0.05, so there is an  association between
# fields of study and funding source from parents/family/spouse

XsqFund <- chisq.test(fund)
XsqFund

# ================================ 9.45 =====================================================
horror <- rbind(c(68, 7, 64, 32), c(39, 4, 83, 43), c(69, 13, 37, 31), c(40, 0, 27, 7),
                c(64, 0, 64, 50), c(75, 17, 50, 8), c(55, 0, 64, 27))

colnames(horror) <- c("bedtime shorterm", "bedtime enduring", "waking shorterm", "waking enduring")
rownames(horror) <- c("Poltergeist (n 29)", "Jaws (n 23)", "Nightmare on Elm Street (n 16)",
                      "Thriller (music video) (n 16)", "It (n 24)", "The Wizard of Oz (n 12)",
                      "E.T. (n 11)")

# the chisq test is not approriate because there is an expected value that is smaller than 5
horror <- as.table(horror)
XsqHorror <- chisq.test(horror)
XsqHorror
