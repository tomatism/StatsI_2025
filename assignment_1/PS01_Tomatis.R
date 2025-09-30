#####################
# load libraries
# set wd
# clear global .envir
#####################

# set working directory 
setwd("C:\\Users\\matil\\Documents\\GitHub\\StatsI_2025\\assignment_1")

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

lapply(c("ggplot2", "tidyverse", "patchwokr"),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
n <- length(na.omit(y))

# Step 1: Finding the sample mean and sd for the student IQ 
# and storing them in a variable (common for point 1 and 2)
avg_IQ <- mean(y) 
#The value calculated is 98.44
sample_sd <- sd(y)
#13.09287

#note: we don't have na


##1
#Step 1.2: Finding the .90 t-value 
# We the sample is smaller than 30, df = n -1 = 25 - 1 = 24
t90 <- qt((1-0.90)/2, df = 24, lower.tail = FALSE)
#The corresponding t-value is 1.710882

#Step 1.3: Finding the lower and upper tail
#We will find the SE * t and + / - it from the average 

SE <- sample_sd / sqrt(n)
lower_90 <- avg_IQ - t90 * SE 
upper_90 <- avg_IQ + t90 * SE
CI_IQ <- c(lower_90,upper_90)
# CI(93.95993, 102.92007)

## 2

#Stating hypothesis: 
#Ho: avg_IQ <= 100 // Ha: avg_IQ > 100
#Finding appropriate test stat and the associated p-value

test_stat <- (avg_IQ - 100) / SE
#the test statistic is equal to -0.5957439
p_value <- pt(test_stat, df = 24, lower.tail =  FALSE)

#Evaluating the results
alpha <- 0.05
result <- ifelse(p_value > alpha, 
                 paste("p-value:", p_value, "- Ho cannot be rejected"),
                 paste("p-value:", p_value, "- Ho is rejected"))
print(result)

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
str(expenditure)

## 1
#Plotting each variable with another with ggplot + their correlation
#Saving them all as pdf for latex 

corYX1  <- cor(expenditure$Y, expenditure$X1)
corYX2  <- cor(expenditure$Y, expenditure$X2)
corYX3  <- cor(expenditure$Y, expenditure$X3)
corX1X2 <- cor(expenditure$X1, expenditure$X2)
corX2X3 <- cor(expenditure$X2, expenditure$X3)

Y_X1 <- ggplot(expenditure, aes(X1, Y)) +
  geom_point() +
  annotate("text",
           x = 1300, y = 120,
           label = paste0("corr = ", round(corYX1, 4)),
           hjust = 1.1, vjust = 1.5, size = 2.5) +
  theme_bw()

Y_X2 <- ggplot(expenditure, aes(X2, Y)) +
  geom_point() +
  annotate("text",
           x = 180, y = 120,
           label = paste0("corr = ", round(corYX2, 4)),
           hjust = 1.1, vjust = 1.5, size = 2.5) +
  theme_bw()

Y_X3 <- ggplot(expenditure, aes(X3, Y)) +
  geom_point() +
  annotate("text",
           x = 400, y = 120,
           label = paste0("corr = ", round(corYX3, 4)),
           hjust = 1.1, vjust = 1.5, size = 2.5) +
  theme_bw()

X1_X2 <- ggplot(expenditure, aes(X1, X2)) +
  geom_point() +
  annotate("text",
           x = 1300, y = 500,
           label = paste0("corr = ", round(corX1X2, 4)),
           hjust = 1.1, vjust = 1.5, size = 2.5) +
  theme_bw()


X2_X3 <- ggplot(expenditure, aes(X2, X3)) +
  geom_point() +
  annotate("text",
           x = 160, y = 880,
           label = paste0("corr = ", round(corX2X3, 4)),
           hjust = 1.1, vjust = 1.5, size = 2.5) +
  theme_bw()

combined_plots <- (Y_X1| Y_X2| Y_X3) /
  (X1_X2 | X2_X3)
pdf("combined_plots.pdf")
print(combined_plots)
dev.off()


##2
#Checking the nature of the variable 
str(expenditure$Region)
#Regions are now as integers, but they can be seen as 4-levels factors
#Re-coding them as such 

Regions_Names <- c("Northeast", "North Central", 
                   "South", "West")
expenditure$Fact_Region <- factor(expenditure$Region,
                                   levels = 1:4,
                                  labels = Regions_Names)
str(expenditure$Fact_Region)
#Checking everything was correctly coded in each level
table(expenditure$Region, expenditure$Fact_Region)

#Calculating the average for each factor
tapply(expenditure$Y, expenditure$Fact_Region, mean)
#Here are the results:  Northeast 79.44444  
# North Central  83.91667 South 69.18750 West 88.30769 


#Plotting the relationship between the two, different colors based on the level
# + adding the mean as visual representation
Y_Region <- ggplot(expenditure, aes(x = Fact_Region, y = Y, color = Fact_Region)) +
  geom_point() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "black") +
  theme_bw()
pdf("Y_Region.pdf")
print(Y_Region)
dev.off()

## 3
#Plotting X and Y, with different colors and shapes based on the region of origin
Y_X1_R <- ggplot(expenditure, aes(X1, Y, 
                      colour = Fact_Region, shape = Fact_Region )) +
  geom_point() +
  theme_bw()
pdf("Y_X1_R.pdf")
print(Y_X1_R)
dev.off()

#Calculating the correlation for each level 
# note: (I used tidyverse as I didn't know how to do it with base R)
expenditure |> 
  group_by(Fact_Region) |> 
  summarise(correlation = cor(X1, Y))

