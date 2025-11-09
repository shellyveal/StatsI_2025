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
library("stargazer")
lapply("tidyverse", pkgTest)
theme_set(theme_minimal())
# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc_sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

glimpse(inc_sub)
view(inc_sub)

# Question 1 --------------------------------------------------------------
# - 1.1 -

reg_line_1 <- lm(inc_sub$voteshare ~ inc_sub$difflog)
reg_line_1

stargazer(reg_line_1, 
          type = "latex",
          title = "Incumbent Vote Share Explained by Spending",
          column.labels = "Coefficients (Reg 1)",
          covariate.labels = "Spending (Difflog)",
          dep.var.labels = "Voteshare")

# - 1.2 -

plot_1 <- ggplot(inc_sub, aes(x = difflog,
                    y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Spending (Difflog)",
       y = "Incumbent Vote Share")
plot_1
ggsave("plot_1.pdf", plot = plot_1, width = 10, height = 6, units = "in", dpi = 300)

# - 1.3 -

residuals_rl_1 <- reg_line_1$residuals
residuals_rl_1


# - 1.4 -

alpha_rl_1 <- reg_line_1$coefficients[1]
beta_rl_1 <- reg_line_1$coefficients[2]

# y = alpha_rl_1 + beta_rl_1*x

p_vs_fun <- function(x) {
  y <- alpha_rl_1 + beta_rl_1*x
  return(y)
}

for (x in length(inc_sub$difflog)) {
  predicted_vs <- alpha_rl_1 + beta_rl_1*inc_sub$difflog
}
predicted_vs
p_vs_fun(5)


# Question 2 --------------------------------------------------------------
# - 2.1 -

reg_line_2 <- lm(inc_sub$presvote ~ inc_sub$difflog)
reg_line_2

stargazer(reg_line_2, 
          type = "latex",
          title = "Presidental Vote Share Explained by Spending",
          column.labels = "Coefficients (Reg 2)",
          covariate.labels = "Spending (Difflog)",
          dep.var.labels = "Presvote")

# - 2.2 -

plot_2 <- ggplot(inc_sub, aes(x = difflog,
                    y = presvote))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Spending (Difflog)",
       y = "Presidential Vote Share")
ggsave("plot_2.pdf", plot = plot_2, width = 10, height = 6, units = "in", dpi = 300)

# - 2.3 -

residuals_rl_2 <- reg_line_2$residuals

# - 2.4 -

alpha_rl_2 <- reg_line_2$coefficients[1]
beta_rl_2 <- reg_line_2$coefficients[2]

# y = alpha_rl_2 + beta_rl_2*x

p_vs_fun_2 <- function(x) {
  y <- alpha_rl_2 + beta_rl_2*x
  return(y)
}

for (x in length(inc_sub$difflog)) {
  predicted_vs_2 <- alpha_rl_2 + beta_rl_2*inc_sub$difflog
}
predicted_vs_2
p_vs_fun_2(5)


# Question 3 --------------------------------------------------------------
# - 3.1 -

reg_line_3 <- lm(inc_sub$voteshare ~ inc_sub$presvote)
reg_line_3

stargazer(reg_line_3, 
          type = "latex",
          title = "Incumbent Vote Share Explained by Presidential Vote Share",
          column.labels = "Coefficients (Reg 3)",
          covariate.labels = "Presvote",
          dep.var.labels = "Voteshare")

# - 3.2 -

plot_3 <- ggplot(inc_sub, aes(x = presvote,
                    y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Presidential Vote Share",
       y = "Incumbent Vote Share")
ggsave("plot_3.pdf", plot = plot_3, width = 10, height = 6, units = "in", dpi = 300)

# - 3.3 -

alpha_rl_3 <- reg_line_3$coefficients[1]
beta_rl_3 <- reg_line_3$coefficients[2]

# y = alpha_rl_3 + beta_rl_3*x

p_vs_fun_3 <- function(x) {
  y <- alpha_rl_3 + beta_rl_3*x
  return(y)
}

for (x in length(inc_sub$difflog)) {
  predicted_vs_3 <- alpha_rl_3 + beta_rl_3*inc_sub$presvote
}
predicted_vs_3
p_vs_fun_3(.8)


# Question 4 --------------------------------------------------------------
# - 4.1 -
reg_line_4 <- lm(residuals_rl_1 ~ residuals_rl_2)
reg_line_4

stargazer(reg_line_4, 
          type = "latex",
          title = "Residuals of Reg 1 Explained by Residuals of Reg 2",
          column.labels = "Coefficients (Reg 4)",
          covariate.labels = "Residuals Reg 2",
          dep.var.labels = "Residuals Reg 1")

# - 4.2 -
df <- data.frame(
  "Residuals_1" = residuals_rl_1,
  "Residuals_2" = residuals_rl_2
)

plot_4 <- ggplot(df, aes(x = Residuals_2,
                    y = Residuals_1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Residuals of Regression 2",
       y = "Residuals of Regression 1")
ggsave("plot_4.pdf", plot = plot_4, width = 10, height = 6, units = "in", dpi = 300)

# - 4.3 -

alpha_rl_4 <- reg_line_4$coefficients[1]
beta_rl_4 <- reg_line_4$coefficients[2]

# y = alpha_rl_4 + beta_rl_4*x

p_vs_fun_4 <- function(x) {
  y <- alpha_rl_4 + beta_rl_4*x
  return(y)
}

for (x in length(df$Residuals_2)) {
  predicted_vs_4 <- alpha_rl_4 + beta_rl_4*df$Residuals_2
}
predicted_vs_4
p_vs_fun_4(.4)


# Question 5 --------------------------------------------------------------
# - 5.1 -

reg_line_5 <- lm(inc_sub$voteshare ~ inc_sub$difflog + inc_sub$presvote, data = inc_sub)
reg_line_5

stargazer(reg_line_5, 
          type = "latex",
          title = "Multiple Regression: Incumbent Vote Share Explained by Spending and Presidential Vote Share",
          column.labels = "Coefficients (Reg 5)",
          covariate.labels = c("Spending (Difflog)", "Presvote"),
          dep.var.labels = "Voteshare")

# - 5.2 -

alpha_rl_5 <- reg_line_5$coefficients[1]
beta_rl_5_difflog <- reg_line_5$coefficients[2]
beta_rl_5_presvote <- reg_line_5$coefficients[3]
# predicted voteshare = alpha_rl_5 + beta_rl_5_difflog*x1 + beta_rl_5_presvote*x2

p_vs_fun_5 <- function(diff_log, pres_vote) {
  y <- alpha_rl_5 + beta_rl_5_difflog*diff_log + beta_rl_5_presvote*pres_vote
  return(y)
}

for (x in length(df$voteshare)) {
  predicted_vs_5 <- alpha_rl_5 + beta_rl_5_difflog*inc_sub$difflog + beta_rl_5_presvote*inc_sub$presvote
}
predicted_vs_5

inc_sub$difflog[4]
inc_sub$presvote[4]

p_vs_fun_5(inc_sub$difflog[4], inc_sub$presvote[4])
predicted_vs_5[4]









