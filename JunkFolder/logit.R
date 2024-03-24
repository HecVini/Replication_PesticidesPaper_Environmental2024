library(sandwich)
library(lmtest)
library(fixest)
options(scipen=99)
library(dplyr)
library(tidyr)
library(janitor)
library(haven)


setwd("D:/OneDrive - Fundacao Getulio Vargas - FGV/PhD EPGE/Courses/1T24/Environmental and Urban/replication/Replication_PesticidesPaper_Environmental2024")

bananas1.R <- read_dta("bananas1.dta") # Main dataset used on the papaer

bananas1 <- bananas1.R %>% clean_names() # Clean the column names


model <- feglm(preterm ~ bx+ pxp + bx:pxp +  + age + educ + private +
                 labors + nchild + visits + nbl + sex + tlabor  + as.factor(marital) + as.factor(ethnic)  |  grid + cohort ,
               bananas1, family = 'logit')

summary(model)

# Get coefficient estimates
coef_estimates <- coef(model)

# Compute odds ratios
odds_ratios <- exp(coef_estimates)

# Print odds ratios
print(odds_ratios)
