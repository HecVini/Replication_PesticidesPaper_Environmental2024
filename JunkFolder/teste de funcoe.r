## 3. Setup Functions

# Get model specification and return the formula of it
set_model <- function(data, dependent_var, independent_var, fixed_effects, model_type) {
    dependent_var_str <- dependent_var
    independent_var_str <- paste(independent_var, collapse = " + ")
    fixed_effects_str <- paste(fixed_effects, collapse = " + ")
    mom_controls <- paste(c("age", "educ", "private", "labors", "nchild", "visits", "nbl", "sex", "tlabor", "as.factor(marital)"), collapse = " + ") #Every rergression on the paper use these controls

    # OLS and Logit have a slightly different formula
    if (model_type == "ols") {
        formula_str <- paste(dependent_var_str, "~", independent_var_str, "+", mom_controls, " | ", fixed_effects_str)
        formula <- as.formula(formula_str)
    } else if (model_type == "logit") {
        formula_str <- paste(dependent_var_str, "~", independent_var_str, "+", mom_controls, "|", fixed_effects_str)
        formula <- as.formula(formula_str)
    }
    return(formula)
}

teste1 = set_model(
    data = bananas2,
    model_type = "ols",
    dependent_var = "weight",
    independent_var = c("bx * pxt1 + bx * pxt2 + bx * pxt3",birth_interval),
    fixed_effects = c("cohort", "mfeid*grid"))
teste1
# Make the function to perform bootstrap
boot_coefs <- function(data, formula, model_type) {
    if (model_type == "ols") {
        fit <- felm(formula, data = data)
        return(coef(fit)) # Return the coefficients of the OLS model 
    } else if (model_type == "logit") {
        fit <- feglm(formula, data = data, family = 'logit')
        return(coef(fit)) # Return the coefficients of the Logit model
    }
}

# Perform the bootstrap with clustered standard errors
clusterbootreg_boot <- function(formula, data, cluster, reps, model_type, model_number) {
    # Fit the model to the full data
    if (model_type == "ols") {
        reg_fit <- felm(formula, data)
    } else if (model_type == "logit") {
        reg_fit <- feglm(formula, data, family = 'logit')
    }



    # Combine estimates and standard errors
    estimates <- coef(reg_fit)
    variable_names <- names(estimates)
    results <- tibble(
        variable = variable_names,
        estimate = round(estimates, 2),
        robust_se = boot_se
    )

    results = results %>% mutate( # Add confidence intervals and significance
        ci_lower = estimate - 1.96 * robust_se, 
        ci_upper = estimate + 1.96 * robust_se, 
        significant = ifelse(ci_lower > 0 | ci_upper < 0, "yes", "no"), 
    )

    if(model_type == "logit"){
        results$estimate = exp(results$estimate)
        results$ci_lower = exp(results$ci_lower)
        results$ci_upper = exp(results$ci_upper)

    } else if(model_type == "ols"){
        results = results 
    }

    results = results %>% mutate(
        ci = paste0('[', round(ci_lower, 2), ', ', round(ci_upper, 2), ']')
        )
    results = results %>% subset(select = -c(ci_lower, ci_upper)) # Remove ci_lower and ci_upper columns
    colnames(results) <- c(
        "variable", 
        paste0('estimate_',model_number),
        paste0('robust_se_',model_number),
        paste0('significant_',model_number),
        paste0('ci_',model_number)
        )
    return(results)

}

# Put it all together
run_bootstrap <- function(data, model_type, dependent_var, independent_var, fixed_effects, cluster, reps, model_number) {
    # OLS and Logit need different formulas to run
    if (model_type == "ols") {
        formula <- set_model(
            data = data,
            dependent_var = dependent_var,
            independent_var = independent_var,
            fixed_effects = fixed_effects,
            model_type = "ols"
        )
        bootstrapping <- clusterbootreg_boot(formula, data = data, cluster = cluster, reps = reps, model_type = "ols", model_number = model_number)

    } else if (model_type == "logit") {
        formula <- set_model(
            data = data,
            dependent_var = dependent_var,
            independent_var = independent_var,
            fixed_effects = fixed_effects,
            model_type = "logit"
        )
        bootstrapping <- clusterbootreg_boot(formula, data = data, cluster = cluster, reps = reps, model_type = "logit", model_number = model_number)
    }
    return(bootstrapping)
}


form1 = as.formula(weight  ~ bx:pxt1 + bx:pxt2 + bx:pxt3 + birth_interval1 + birth_interval2 + birth_interval3 + age + educ + private + labors + nchild + visits + nbl + sex + tlabor + as.factor(marital) | cohort + mfeid:grid)
form1
bananas2$mfeid:bananas2$grid
model = felm(formula = form1, data = bananas2)






y = felm(weight ~ bx + birth_interval1 + birth_interval2 + birth_interval3 + age + educ + private + labors + nchild + visits + nbl + sex + tlabor + as.factor(marital) | cohort + grid | 0 | mfeid + grid,
  data = bananas2,
  cmethod = "reghdfe")
summary(y)

y = felm(weight ~ bx + birth_interval1 + birth_interval2 + birth_interval3 + age + educ + private + labors + nchild + visits + nbl + sex + tlabor + as.factor(marital) | cohort + grid, data = bananas2)
y

bananas2
