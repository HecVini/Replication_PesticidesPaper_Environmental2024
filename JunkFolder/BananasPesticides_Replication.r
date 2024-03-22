## 1. Load Packages
library(tidyverse) # Easily Installand Load the 'Tidyverse'
library(lubridate) # Make Dealing with Dates a Little Easier
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(data.table) # Extension of `data.frame`
library(tidylog) # Logging for 'dplyr' and 'tidyr' Functions
library(haven) # Open .xpt and Stata (.do) files
library(foreign) # Read and Write Data from Other Statistical Systems
library(Hmisc) # Harrell Miscellaneous. Used to extract .dta file label easily
library(htmltools) # Tools for HTML
library(lintr) # Trying to removing the annoying lint
library(gt) # Easily Create Presentation-Ready Display Tables
library(gtExtras) # Additional Functions to Enhance 'gt'
library(broom) # Convert Statistical Analysis Objects into Tidy Tibbles
library(boot) # Bootstrap Functions for R
library(lfe) # Linear Group Fixed Effects
library(sandwich) # For robust standard errors
library(lmtest) # For coefficient testing with robust standard errors
library(plm) # Panel Data Models - to perform OLS with FE
library(tidyr)
library(tidycat)
library(bife)
library(fixest)
library(multiwayvcov)
library(progress)

# linter:disable
lint(
    text = strrep("x", 400L),
    linters = line_length_linter(length = 400L)
)

## 2. Load and Clean Data
setwd("C:/Users/vinic/OneDrive/Mestrado/5Tri/EnvironmentalUrban/ReplicationPackage")

bananas1.R <- read_dta("bananas1.dta") # Main dataset used on the papaer
bananas2.R <- read_dta("bananas2.dta") # Dataset used to generate the tables with maternal fixed effects
bananas3.R <- read_dta("bananas3.dta") # Dataset used to generate the tables that examine the effects of pesticides in other crop plantations

bananas1 <- bananas1.R %>% clean_names() # Clean the column names
bananas2 <- bananas2.R %>% clean_names() # Clean the column names
bananas3 <- bananas3.R %>% clean_names() # Clean the column names

GetBananasVariableDescription <- function(dataset) {
    dataset_length <- dim(dataset)[2] # Get the number of rows
    dataset_labels <- sapply(dataset, function(column) attr(column, "label")) # Get the labels of the columns (its a messy list)
    dataset_types <- sapply(dataset, function(column) attr(column, "format.stata"))
    dataset_variables_description <- tibble(column_name = NA, label = NA, format = NA) # Create a tibble to store the variables description
    for (i in c(1:dataset_length)) {
        dataset_variables_description[i, 1] <- names(dataset[i])
        dataset_variables_description[i, 2] <- dataset_labels[[i]]
        dataset_variables_description[i, 3] <- dataset_types[[i]]
    } # complete the tibble with the variables description. First column: each name, second column: each label
    return(dataset_variables_description)
}

bananas1_variables_description <- GetBananasVariableDescription(bananas1)
bananas1_variables_description %>% print()
bananas2_variables_description <- GetBananasVariableDescription(bananas2)
bananas3_variables_description <- GetBananasVariableDescription(bananas3) 

bananas1$marital <- as.factor(bananas1$marital)
bananas1$ethnic <- as.factor(bananas1$ethnic)
bananas1$grid <- as.factor(bananas1$grid)
bananas1$cohort <- as.factor(bananas1$cohort)

bananas2$marital <- as.factor(bananas2$marital)
bananas2$ethnic <- as.factor(bananas2$ethnic)
bananas2$grid <- as.factor(bananas2$grid)
bananas2$cohort <- as.factor(bananas2$cohort)
bananas2$mfeid <- as.factor(bananas2$mfeid)
bananas2 = bananas2 %>% mutate(mfeid_grid = interaction(mfeid, grid)) %>% drop_na(mfeid_grid)

x = bananas2 %>% subset(select = c(mfeid,grid,mfeid_grid))
length(unique(x$mfeid_grid))

#bananas2 = bananas2 %>% mutate(mfeid_grid = interaction(mfeid, grid)) %>% drop_na(mfeid_grid)

no_bootstraps = 10 # Set here the number of bootstraps for the whole code

## 3. Setup Functions

# Get model specification and return the formula of it
set_model <- function(data, dependent_var, independent_var, fixed_effects, model_type) {
    dependent_var_str <- dependent_var
    independent_var_str <- paste(independent_var, collapse = " + ")
    fixed_effects_str <- paste(fixed_effects, collapse = " + ")
    mom_controls <- paste(c("age", "educ", "private", "labors", "nchild", "visits", "nbl", "sex", "tlabor", "marital"), collapse = " + ") #Every rergression on the paper use these controls

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

# Make the function to perform bootstrap
boot_coefs <- function(data, indices, formula, model_type) {
    d <- data[indices, ] #d: current bootstrap sample

    if (model_type == "ols") {
        fit <- felm(formula, data = d)
        return(coef(fit)) # Return the coefficients of the OLS model 
    } else if (model_type == "logit") {
        fit <- feglm(formula, data = d, family = 'logit')
        return(coef(fit)) # Return the coefficients of the Logit model
    }
}

# Perform the bootstrap with clustered standard errors
clusterbootreg_boot <- function(formula, data, cluster, reps, model_type, model_number) {
    cluster_indices <- match(cluster, unique(cluster)) # cluster_indices: vector of indices representing each unique value in the cluster variable.

    boot_results <- boot(data,
        statistic = function(data, indices) boot_coefs(data, indices, formula, model_type),
        R = reps, strata = cluster_indices,
        parallel = "multicore", ncpus = 4) # Use boot() to perform bootstrap using the cluster variable as stratification 

    boot_se <- apply(boot_results$t, 2, sd) # Calculate standard errors
    
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
        estimate = round(estimates, 3),
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

## 4. Table 2: summary statistics

## 5. Table 3: Effects of the Seasonal Intensification of Fumigations on Birth Weight
x = run_bootstrap(
    data = bananas1,
    model_type = "ols",
    dependent_var = "weight",
    independent_var = c("bx"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = 20,
    model_number = 1
)
x

table3_2 = run_bootstrap(
    data = bananas1,
    model_type = "ols",
    dependent_var = "weight",
    independent_var = c("bx * pxp"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 2
)

table3_3 = run_bootstrap(
    data = bananas1,
    model_type = "ols",
    dependent_var = "weight",
    independent_var = c("bx * pxt1","ethnic"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 3
)

table3_4 = run_bootstrap(
    data = bananas1,
    model_type = "ols",
    dependent_var = "weight",
    independent_var = c("bx * pxt2"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 4
)

table3_5 = run_bootstrap(
    data = bananas1,
    model_type = "ols",
    dependent_var = "weight",
    independent_var = c("bx * pxt3"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 5
)

table3_6 = run_bootstrap(
    data = bananas1,
    model_type = "ols",
    dependent_var = "weight",
    independent_var = c("bx * pxt1 + bx * pxt2 + bx * pxt3"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 6
)

table3_results = full_join(table3_1, table3_2, by = "variable") %>% 
    full_join(table3_3, by = "variable") %>% 
    full_join(table3_4, by = "variable") %>% 
    full_join(table3_5, by = "variable") %>% 
    full_join(table3_6, by = "variable") # Join all the results

table3_results = table3_results[c(1,24,25,26,28,30,27,29,31),] # Reorder the rows (excl. mom controls)

## 6. Table 4: Effects of the Seasonal Intensification of Fumigations on Gestation Weeks, Preterm, and Low Birth Weight
table4_1 = run_bootstrap(
    data = bananas1,
    model_type = "ols",
    dependent_var = "gweeks",
    independent_var = c("bx * pxp"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 1
)

table4_2 = run_bootstrap(
    data = bananas1,
    model_type = "ols",
    dependent_var = "gweeks",
    independent_var = c("bx * pxt1 + bx * pxt2 + bx * pxt3"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 2
)

table4_3 = run_bootstrap(
    data = bananas1,
    model_type = "logit",
    dependent_var = "preterm",
    independent_var = c("bx + pxp + bx*pxp"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = 10,
    model_number = 3
)

table4_4 = run_bootstrap(
    data = bananas1,
    model_type = "logit",
    dependent_var = "preterm",
    independent_var = c("bx * pxt1 + bx * pxt2 + bx * pxt3"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 4
)

table4_5 = run_bootstrap(
    data = bananas1,
    model_type = "logit",
    dependent_var = "lbw",
    independent_var = c("bx * pxp"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 5
)

table4_6 = run_bootstrap(
    data = bananas1,
    model_type = "logit",
    dependent_var = "lbw",
    independent_var = c("bx * pxt1 + bx * pxt2 + bx * pxt3"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 6
)

## 7. Table 5: Effects of the Seasonal Intensification of Fumigations on Birth Weight and Gestation Weeks: Maternal Fixed Effects
birth_interval = c("birth_interval1","birth_interval2","birth_interval3")
table5_1 = run_bootstrap(
    data = bananas2,
    model_type = "ols",
    dependent_var = "weight",
    independent_var = c("bx",birth_interval),
    fixed_effects = c("cohort","mfeid*grid"),
    cluster = bananas2$mfeid_grid,
    reps = no_bootstraps,
    model_number = 1
)
table5_1
table5_2 = run_bootstrap(
    data = bananas2,
    model_type = "ols",
    dependent_var = "weight",
    independent_var = c("bx * pxp",birth_interval),
    fixed_effects = c("mfeid_grid"),
    cluster = bananas2$mfeid_grid,
    reps = no_bootstraps,
    model_number = 2
)

table5_3 = run_bootstrap(
    data = bananas2,
    model_type = "ols",
    dependent_var = "weight",
    independent_var = c("bx * pxt1 + bx * pxt2 + bx * pxt3",birth_interval),
    fixed_effects = c("mfeid_grid"),
    cluster = bananas2$mfeid_grid,
    reps = no_bootstraps,
    model_number = 3
)

table5_4 = run_bootstrap(
    data = bananas2['stay_mom' > 0,],
    model_type = "ols",
    dependent_var = "gweeks",
    independent_var = c("bx:pxt1 + bx*pxt2 + bx*pxt3",birth_interval),
    fixed_effects = c('cohort',"mfeid_grid"),
    cluster = bananas2$mfeid_grid,
    reps = 2,
    model_number = 4
)

table5_5 = run_bootstrap(
    data = bananas2,
    model_type = "ols",
    dependent_var = "gweeks",
    independent_var = c("bx",birth_interval),
    fixed_effects = c("mfeid_grid"),
    cluster = bananas2$mfeid_grid,
    reps = no_bootstraps,
    model_number = 5
)

table5_6 = run_bootstrap(
    data = bananas2,
    model_type = "ols",
    dependent_var = "gweeks",
    independent_var = c("bx * pxp",birth_interval),
    fixed_effects = c("mfeid_grid"),
    cluster = bananas2$mfeid_grid,
    reps = no_bootstraps,
    model_number = 6
)

table5_7 = run_bootstrap(
    data = bananas2,
    model_type = "ols",
    dependent_var = "gweeks",
    independent_var = c("bx * pxt1 + bx * pxt2 + bx * pxt3",birth_interval),
    fixed_effects = c("mfeid_grid"),
    cluster = bananas2$mfeid_grid,
    reps = no_bootstraps,
    model_number = 7
)

table5_8 = run_bootstrap(
    data = bananas2['stay_mom' > 0,],
    model_type = "ols",
    dependent_var = "preterm",
    independent_var = c("bx * pxp",birth_interval),
    fixed_effects = c("mfeid_grid"),
    cluster = bananas2$mfeid_grid,
    reps = no_bootstraps,
    model_number = 8
)
## 8. Table 6: Effects of the Seasonal Intensification of Fumigations on Maternal Characteristics: Main Estimation Approach
table6_1 = run_bootstrap(
    data = bananas1,
    model_type = "ols",
    dependent_var = "labors",
    independent_var = c("bx*pxp"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = 50,
    model_number = 1
)
table6_1
table(bananas1$nchild)
table6_2 = run_bootstrap(
    data = bananas1,
    model_type = "ols",
    dependent_var = "nchild",
    independent_var = c("bx * pxp"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 2
)

table6_3 = run_bootstrap(
    data = bananas1,
    model_type = "ols",
    dependent_var = "nbl",
    independent_var = c("bx * pxp"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 3
)

table6_4 = run_bootstrap(
    data = bananas1,
    model_type = "ols",
    dependent_var = "visits",
    independent_var = c("bx * pxp"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 4
)

table6_5 = run_bootstrap(
    data = bananas1,
    model_type = "logit",
    dependent_var = "educ",
    independent_var = c("bx * pxp"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 5
)

table6_6 = run_bootstrap(
    data = bananas1,
    model_type = "logit",
    dependent_var = "private",
    independent_var = c("bx * pxp"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 6
)

table6_7 = run_bootstrap(
    data = bananas1,
    model_type = "logit",
    dependent_var = "tlabor",
    independent_var = c("bx * pxp","ethnic"),
    fixed_effects = c("grid", "cohort"),
    cluster = bananas1$grid,
    reps = no_bootstraps,
    model_number = 7
)
table6_7
