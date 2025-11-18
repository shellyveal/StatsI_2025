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
lapply(c("car", "tidyverse","stargazer"),  pkgTest)
theme_set(theme_minimal())
options(scipen = 999)
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data(Prestige)
help(Prestige)
view(Prestige)

# Question 1 --------------------------------------------------------------

## - 1a - ##
# remove NA's so we only compare professional, White Collar, and Blue Collar
df <- Prestige[!is.na(Prestige$type), ] 
# create "professional" variable
df$professional <- ifelse(df$type == "prof", 1, 0) 

view(df)

## - 1b - ##
plot_income <- ggplot(df, aes(x = income,
                              y = prestige)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Income ($)",
       y = "Prestige") +
  ylim(0, 100)
plot_income
ggsave("plot_1.pdf", plot = plot_income, width = 10, height = 6,
       units = "in", dpi = 300)


plot_prof <- ggplot(df, aes(x = professional,
                         y = prestige)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Professional",
       y = "Prestige") +
  ylim(0, 100)
plot_prof
ggsave("plot_2.pdf", plot = plot_prof, width = 10, height = 6,
       units = "in", dpi = 300)

inc_prof <- lm(df$prestige ~ df$income + df$professional)
summary(inc_prof)
stargazer(inc_prof, 
          type = "latex",
          title = "Impact of Income and Professionality on Prestige",
          covariate.labels = c("Income", "Professional"),
          dep.var.labels = "Prestige")




## - 1c - ##
# df$prestige = 30.618 + 1.371e-03(df$income) + 22.756(df$professional)
30.618 + 1.371*9 + 22.756

## - 1d - ##
# see latex/pdf
## - 1e - ##
# see latex/pdf
## - 1f - ##
co_inc_prof <- inc_prof$coefficients
slope_inc <- co_inc_prof[2]

ans_1f <- slope_inc*1000
ans_1f

## - 1g - ##
slope_prof <- co_inc_prof[3]
ans_1g <- slope_prof
ans_1g

# note - the question is phrased poorly for the data:
# really what we're finding here is the difference of prestige for
# occupations of different professionality but with the same income,
# not the change in prestige a specific person might experience by changing 
# their occupation "from non-professional to professional"


# Question 2 --------------------------------------------------------------
## 2a ##
alpha <- 0.302
se_alpha <- 0.011

assigned <- 0.042
se_assigned <- 0.016

adjacent <- 0.042
se_adjacent <- 0.013

n_assigned <- 30
n_adjacent <- 76
n_total <- 131
r_squared <- 0.094

# H0: assigned = 0,  HA: assigned != 0
t_assigned <- (assigned - 0)/se_assigned
p_assigned <- 2*pt(abs(t_assigned), n_total-2, lower.tail = FALSE)
p_assigned

## 2b ##
# H0: adjacent = 0,  HA: adjacent != 0
t_adjacent <- (adjacent - 0)/se_adjacent
p_adjacent <- 2*pt(abs(t_adjacent), n_total-2, lower.tail = FALSE)
p_adjacent

## 2c ##
# see latex/pdf
## 2d ##
# r_squared being 0.094 shows that signs
# only explain nearly 10% of the variance in the data

t_adjacent



