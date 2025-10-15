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
# lapply(c("stringr"),  pkgTest)

lapply(c("tidyverse", "corrplot"),  pkgTest)
theme_set(theme_minimal())
getwd()

#####################
# Problem 1
#####################
# Question 1.a

tab_1 <- matrix(c(14, 7, 6, 7, 7, 1), nrow = 2, ncol = 3)
rownames(tab_1) <- c("Upper class", "Lower class")
colnames(tab_1) <- c("Not Stopped", 
                       "Bribe Requested", 
                       "Stopped/Given Warning")
tab_1m <- addmargins(tab_1)
tab_1m

round(addmargins(prop.table(tab_1)), 2)

# f_exp = rowtot*coltot/grandtot

gt <- tab_1m[3,4]
gt

tab_1m[3, 1]*tab_1m[1, 4]/gt
UC_tab <- tab_1m[-2, ]

UC_tab[2, 1]* UC_tab[1,4]/gt
UC_vec <- for(i in UC_tab[1, ]) {
  
}

# Expected Values:

ns_uc <- 21*27/42
ns_lc <- 21*15/42

br_uc <- 13*27/42
br_lc <- 13*15/42

gw_uc <- 8*27/42
gw_lc <- 8*15/42

exp_values <- c(ns_uc, ns_lc, br_uc, br_lc, gw_uc, gw_lc)

chi_stat <- sum((tab_1 - exp_values)^2/exp_values)
chi_stat

chisq.test(tab_1)

# Question 1.b

round(pchisq(chi_stat, df = 2, lower.tail = FALSE), 4)

# p-value is 0.15, which for alpha .1 is not stat significant

# Question 1.c

exp_values
obs_values <- as.vector(tab_1)
obs_values

num <- obs_values - exp_values

tab_1m

row_props <- 1-(rowSums(tab_1)/gt)
col_props <- 1-(colSums(tab_1)/gt)

glimpse(row_props)

outer(row_props, col_props, "*")

den <- sqrt(exp_values*(outer(row_props, col_props, "*")))

residuals <- round(num/den, 3)
residuals

# Question 1.d


#####################
# Problem 2
#####################

df <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

glimpse(df)
glimpse(df[df$female == 1, ]$female)
fem <- df[df$female == 1, ]$water
fem


corrplot(df, method = "pie")
unique(df$GP)

# Question 2.a

# H0: 


