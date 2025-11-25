#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
lapply(c("car", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## ECONOMICS ##


library(car)
data("Prestige")
View(Prestige$type)
View(Prestige)

# E.a
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# E.b

model_econ <- lm(prestige ~ income * professional, data = Prestige)

stargazer(model_econ,
          type = "latex",
          title = "Economics multivariate regression",
          covariate.labels = c("Income level", 
                               "Professional occupation"),
          dep.var.labels = "Prestige of one's occupation")
summary(model_econ)

# E.f

increase_income <- data.frame(
  income = c(2000, 3000),
  professional = c(1, 1)
)
# With mock data we compute the difference in the predicted prestige
  # for professional with a 1000 dollars difference in income
predictions_income <- predict(model_econ, newdata = increase_income)
difference_income <- predictions_income[[2]] - predictions_income[[1]]

coef_income <- coef(model_econ)["income"]
coef_interaction <- coef(model_econ)["income:professional"]
sum_coef <- coef_income + coef_interaction

# The test is conducted to test wheter is TRUE that the difference
  # and the sum of the coefficients multiplied by 1000 are equal
test <- isTRUE(all.equal(
  as.numeric(difference_income),
  as.numeric(sum_coef) * 1000
))
print(test)

# E.g 

change_professional <- data.frame(
  income = c(6000, 6000),
  professional = c(0, 1)
)
predictions_change <- predict(model_econ, newdata = change_professional)
difference_change <- predictions_change[[2]] - predictions_change[[1]]

coef_professional <- coef(model_econ)["professional"]
coef_interaction <- coef(model_econ)["income:professional"]
sum_coef_2 <- coef_professional + coef_interaction * 6000

test <- isTRUE(all.equal(
  as.numeric(difference_change),
  as.numeric(sum_coef_2)
))
print(test)

## POLITICS ##

# a
t_stat   <- 0.042 / 0.016
df       <- 131 - 2 - 1   # 128
p_value  <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
alpha    <- 0.05
result   <- ifelse(p_value > alpha,
                   paste("p-value:", signif(p_value, 4), "- Ho cannot be rejected"),
                   paste("p-value:", signif(p_value, 4), "- Ho is rejected"))
print(result)

# b
t_stat_b  <- 0.042 / 0.013
df       <- 131 - 2 - 1 
p_value_b <- 2 * pt(abs(t_stat_b), df = df, lower.tail = FALSE)
result_b  <- ifelse(p_value_b > alpha,
                    paste("p-value:", signif(p_value_b, 4), "- Ho cannot be rejected"),
                    paste("p-value:", signif(p_value_b, 4), "- Ho is rejected"))
print(result_b)
