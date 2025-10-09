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

library(tidyverse)
library(corrplot)

#####################
# Problem 1
#####################

# Question 1.1 

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# sample mean
mean_y <- sum(y)/length(y)
mean_y

# sample median
sorted_y <- sort(y, decreasing = FALSE)
sorted_y
median_y <- y[(length(y)%/%2)+1]
median_y

# sample variance
var_y <- (sum((y - mean_y)^2)/(length(y)-1))
var_y

# sample standard deviation
sd_y <- sqrt(var_y)
sd_y

# sample standard error
se_y <- sd_y/sqrt(length(y))
se_y

# assuming normal distribution of IQs, 90% CI:
# z-score for 90% CI normal distribution = 1.645

ci_90_lower <- mean_y - (1.645 * se_y)
ci_90_upper <- mean_y + (1.645 * se_y)

ci_90_lower
mean_y
ci_90_upper

# Question 1.2

# H0: country mean (mean_country = 100) >= sample mean (mean_y)
# HA: country mean (mean_country = 100) < sample mean (mean_y) 

test_statistic <- (mean_y - 100)/se_y
test_statistic

# t value for one sided test is 1.711

pt(abs(test_statistic), df = 24, lower.tail = FALSE)

# fail to reject the null hypothesis, as pt = 0.28 > alpha (0.05)

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)

################ - Data Vis - #######################

head(expenditure)
summary(expenditure)
str(expenditure)

ggplot(expenditure, aes(x = X1,
                        y = Y,
                        color = factor(Region))) +
  geom_point(size = 2) +
  scale_color_manual(values = c("red", "purple", "lightgreen", "blue"),
                     labels = c("1" = "Northeast",
                                "2" = "North Central",
                                "3" = "South",
                                "4" = "West")) +
  labs(title = "Assistance Expenditure (Per Capita) vs Income",
       x = "Personal Income",
       y = "Housing Assistance Expenditure",
       color = "Region")

ggplot(expenditure, aes(x = X2,
                        y = Y,
                        color = factor(Region))) +
  geom_point(size = 2) +
  scale_color_manual(values = c("red", "purple", "lightgreen", "blue"),
                     labels = c("1" = "Northeast",
                                "2" = "North Central",
                                "3" = "South",
                                "4" = "West")) +
  labs(title = "Assistance Expenditure (Per Capita) vs Financial Security",
       x = "Financially Secure Residents (per 100,000)",
       y = "Housing Assistance Expenditure",
       color = "Region")

ggplot(expenditure, aes(x = X3,
                        y = Y,
                        color = factor(Region))) +
  geom_point(size = 2) +
  scale_color_manual(values = c("red", "purple", "lightgreen", "blue"),
                     labels = c("1" = "Northeast",
                                "2" = "North Central",
                                "3" = "South",
                                "4" = "West")) +
  labs(title = "Assistance Expenditure (Per Capita) vs Urban Location",
       x = "Residents Living in Urban Areas (per 1,000)",
       y = "Housing Assistance Expenditure",
       color = "Region")


################ - Question 2.1 - #######################

cor_expenditure <- cor(expenditure[ , c("Y", "X1", "X2", "X3")])

head(cor_expenditure)
str(cor_expenditure)
class(cor_expenditure)

library(corrplot)
?corrplot
corrplot(cor_expenditure, method = "pie")
dev.off()

################ - Question 2.2 - #######################

?aggregate

regional_expenditure <- aggregate(Y ~ Region,
                                  data = expenditure,
                                  sum, na.rm = TRUE)
regional_expenditure

ggplot(regional_expenditure, aes(x = factor(Region),
                                 y = Y,
                                 fill = factor(Region)))+
         geom_col() +
  labs(x = "Region",
       y = "Housing Assistance Expenditure",
       title = "Total Expenditure by Region",
       fill = "Region",
  ) +
  scale_fill_discrete(name = "Region", 
                      labels = c("Northeast",
                                 "North Central",
                                 "South",
                                 "West"))
  
  
?labs

# this plot is fine for looking at totals, but it doesn't
# provide a parameter (mean, e.g.) for comparison across regions

regional_averages <- aggregate(Y ~ Region,
                               data = expenditure,
                               mean, na.rm = TRUE)
regional_averages

ggplot(regional_averages, aes(x = factor(Region),
                                 y = Y,
                                 fill = factor(Region)))+
  geom_col(show.legend = FALSE) +
  labs(x = "Region",
       y = "Average Housing Assistance Expenditure",
       title = "Average Expenditure by Region",
  ) +
  scale_fill_discrete(labels = c("Northeast",
                                 "North Central",
                                 "South",
                                 "West")) +
  scale_x_discrete(labels = c("Northeast",
                              "North Central",
                              "South",
                              "West"))

# finally, ordered neatly (had to adjust labeling)

ggplot(regional_averages, aes(x = reorder(factor(Region), -Y),
                              y = Y,
                              fill = factor(Region)))+
  geom_col(show.legend = FALSE) +
  labs(x = "Region",
       y = "Average Housing Assistance Expenditure",
       title = "Average Expenditure by Region",
  ) +
  scale_fill_discrete(labels = c("1" = "Northeast",
                                 "2" = "North Central",
                                 "3" = "South",
                                 "4" = "West")) +
  scale_x_discrete(labels = c("1" = "Northeast",
                              "2" = "North Central",
                              "3" = "South",
                              "4" = "West"))
dev.off()

################ - Question 2.3 - #######################

# first, a look at the data.

ggplot(expenditure, aes(x = X1,
                        y = Y,
                        color = factor(Region))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "purple", "lightgreen", "blue"),
                     labels = c("1" = "Northeast",
                                "2" = "North Central",
                                "3" = "South",
                                "4" = "West")) +
  labs(title = "Assistance Expenditure (Per Capita) vs Income",
       x = "Personal Income",
       y = "Housing Assistance Expenditure",
       color = "Region")

# next, add regression line

ggplot(expenditure, aes(x = X1,
                        y = Y,
                        color = factor(Region))) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "seagreen") +
  scale_color_manual(values = c("red", "purple", "lightgreen", "blue"),
                     labels = c("1" = "Northeast",
                                "2" = "North Central",
                                "3" = "South",
                                "4" = "West")) +
  labs(title = "Assistance Expenditure (Per Capita) vs Income",
       x = "Personal Income",
       y = "Housing Assistance Expenditure",
       color = "Region")

# then add shapes

fac_region <- factor(expenditure$Region)

ggplot(expenditure, aes(x = X1,
                        y = Y,
                        color = fac_region,
                        shape = fac_region)) +
  geom_point(size = 3) +
  geom_smooth(inherit.aes = FALSE,
              aes(x = X1, y = Y),
              method = "lm", 
              se = FALSE, 
              color = "seagreen") +
  scale_color_manual(values = c("red", "purple", "lightgreen", "blue"),
                     labels = c("1" = "Northeast",
                                "2" = "North Central",
                                "3" = "South",
                                "4" = "West")) +
  scale_shape_manual(values = c(15, 16, 17, 18),
                     labels = c("1" = "Northeast",
                                "2" = "North Central",
                                "3" = "South",
                                "4" = "West")) +
  labs(title = "Assistance Expenditure (Per Capita) vs Income",
       x = "Personal Income",
       y = "Housing Assistance Expenditure",
       color = "Region",
       shape = "Region") +
  guides(color = guide_legend("Region"),
         shape = guide_legend("Region"))
dev.off()
