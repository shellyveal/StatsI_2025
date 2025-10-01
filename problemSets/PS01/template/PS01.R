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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# sample mean
mean_y <- sum(y)/length(y)

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


#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
