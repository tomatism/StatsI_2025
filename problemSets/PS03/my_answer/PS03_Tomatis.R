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
lapply(c("stringr", "ggplot", "tidyverse", "stargazer"), pkgTest)

# set wd for current folder
setwd("C:/Users/matil/Documents/GitHub/StatsI_2025/problemSets/PS03/my_answer")

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")
# View(inc.sub)
## QUESTION 1 ##

# 1.1 #

question_1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(question_1)
stargazer(question_1,
          type = "latex",
          title = "First bivariate regression model",
          covariate.labels = "Difference in campaign respending",
          dep.var.labels = "Incumbent's party overall vote share")

# 1.2 #
windows()
q1_scatter <- ggplot(inc.sub, aes(difflog, voteshare)) +
  geom_point(size = 1.25, alpha = 0.60) +
  geom_smooth(method = "lm",
              formula = y ~ x) +
  theme_bw()
q1_scatter

pdf("q1_scatter.pdf")
print(q1_scatter)
dev.off()

# 1.3 #

q1_residuals <- question_1$residuals
print(q1_residuals)

## QUESTION 2 ##

# 2.1 #

question_2 <- lm(presvote ~ difflog, data = inc.sub)
summary(question_2)
stargazer(question_2,
          type = "latex",
          title = "Second bivariate regression model",
          covariate.labels = "Difference in campaign respending",
          dep.var.labels = "Presidential candidate's vote share")

# 2.2 #
windows()
q2_scatter <- ggplot(inc.sub, aes(difflog, presvote)) +
  geom_point(size = 1.25, alpha = 0.60) +
  geom_smooth(method = "lm",
              formula = y ~ x) +
  theme_bw()
q2_scatter

pdf("q2_scatter.pdf")
print(q2_scatter)
dev.off()

# 2.3 #

q2_residuals <- question_2$residuals

## Question 3 ## 

# 3.1 #

question_3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(question_3)
stargazer(question_3,
          type = "latex",
          title = "Third bivariate regression model",
          covariate.labels = "Presidential candidate's vote share",
          dep.var.labels = "Incumbent's party overall vote share")

# 3.2 #
windows()
q3_scatter <- ggplot(inc.sub, aes(presvote, voteshare)) +
  geom_point(size = 1.25, alpha = 0.60) +
  geom_smooth(method = "lm",
              formula = y ~ x) +
  theme_bw()
q3_scatter

pdf("q3_scatter.pdf")
print(q3_scatter)
dev.off()

## Question 4 ## 

# 4.1 #

question_4 <- lm(q1_residuals ~ q2_residuals)
summary(question_4)
stargazer(question_4,
          type = "latex",
          title = "Regressing Q1 residuals on Q2 residuals",
          covariate.labels = "Question 2 residuals",
          dep.var.labels = "Question 1 residuals")

# 4.2 #
windows()
q4_scatter <- ggplot(data = data.frame(q1_residuals, q2_residuals),
                     aes(x = q2_residuals, y = q1_residuals)) +
  geom_point(size = 1.25, alpha = 0.60) +
  geom_smooth(method = "lm", 
              formula = y ~ x) +
  labs(
    x = "Residuals from presvote ~ difflog (Q2)",
    y = "Residuals from voteshare ~ difflog (Q1)") +
 theme_bw()
q4_scatter

pdf("q4_scatter.pdf")
print(q4_scatter)
dev.off()


## QUESTION 5 ##

question_5 <- lm(voteshare ~ presvote + difflog, data = inc.sub)
summary(question_5)
stargazer(question_3,
          type = "latex",
          title = "Multivariate regression model",
          covariate.labels = c("Presidential candidate's vote share", 
                               "Difference in campaign spending"),
          dep.var.labels = "Incumbent's party overall vote share")
