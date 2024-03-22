 install.packages("sandwich") # Uncomment and run this line if sandwich package is not installed
 install.packages("lmtest")   # Uncomment and run this line if lmtest package is not installed
library(sandwich)
library(lmtest)
 options(scipen=99)
 
 
 # Fit logistic regression model
model <- glm(preterm ~bx * pxp + age + educ + private +
               labors + nchild + visits + nbl + sex + tlabor + marital + ethnic + factor(cohort) +factor(grid),
             family = binomial,
             data = bananas1)

 
# Teste - sem controle nenhum por caract da mae e sem efeito fixo
model <- glm(preterm ~ bx*pxp ,
             family = binomial,
             data = bananas1)


# Fit logistic regression model
model <- glm(preterm ~bx * pxp + age + educ + private +
               labors + nchild + visits + nbl + sex + tlabor + marital + ethnic,
             family=binomial,
             data=bananas1)

# Fit logistic regression model
model <- glm(preterm ~bx * pxp + age + educ + private +
               labors + nchild + visits + nbl + sex + tlabor + marital +
               ethnic + factor(cohort)+ factor(grid),
             family = binomial,
             data = bananas1)

# Cluster-robust standard errors
robust_se <- coeftest(model, vcov = vcovCR(model, cluster = ~ grid))

# Display results
summary(model)
summary(robust_se)
