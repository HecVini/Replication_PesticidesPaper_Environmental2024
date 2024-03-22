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

# linter:disable
lint(
    text = strrep("x", 400L),
    linters = line_length_linter(length = 400L)
)

## 2. Load Data
setwd("C:/Users/vinic/OneDrive/Mestrado/5Tri/EnvironmentalUrban/ReplicationPackage")

bananas1.R <- read_dta("bananas1.dta") # Main dataset used on the papaer
bananas2.R <- read_dta("bananas2.dta") # Dataset used to generate the tables with maternal fixed effects
bananas3.R <- read_dta("bananas3.dta") # Dataset used to generate the tables that examine the effects of pesticides in other crop plantations

## 3. Get the variables labels
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

## 4. Create the funtions
ols_model <- function(data, dependent_var, independent_vars, fixed_effects) {
    independent_vars_string <- paste(independent_vars, collapse = " + ")
    mom_controls <- paste(c("age", "educ", "private", "labors", "nchild", "visits", "nbl", "sex", "tlabor", "marital", "ethnic"), collapse = " + ")
    fixed_effects_string <- paste(fixed_effects, collapse = " + ")

    formula_string <- paste(dependent_var, "~", independent_vars_string, "+", mom_controls, "|", fixed_effects_string)
    formula <- as.formula(formula_string)

    fit <- felm(formula, data = data)
    return(coef(fit))
}

logit_model <- function(data, dependent_var, independent_vars, fixed_effects) {
    independent_vars_string <- paste(independent_vars, collapse = " + ")
    control_vars_string <- paste(control_vars, collapse = " + ")
    mom_controls <- paste(c("age", "educ", "private", "labors", "nchild", "visits", "nbl", "sex", "tlabor", "marital", "ethnic"), collapse = " + ")
    fixed_effects_string <- paste(fixed_effects, collapse = " + ")

    formula_string <- paste(dependent_var, "~", independent_vars_string, "+", mom_controls, "+", fixed_effects_string)
    formula <- as.formula(formula_string)

    fit <- glm(formula, data = data, family = binomial())
    return(coef(fit))
}

run_bootstrap <- function(data, model_type, dependent_var, independent_vars, fixed_effects, n_bootstraps, model_number) {

    # Bootstrapping function
    boot_statistic <- function(d, indices) {
        d_boot <- d[indices, ]
        if (model_type == "ols") {
            return(ols_model(d_boot, dependent_var, independent_vars, control_vars))
        } else if (model_type == "logit") {
            return(logit_model(d_boot, dependent_var, independent_vars, control_vars))
        } else {
            stop("Invalid model type. Choose 'ols' or 'logit'.")
        }
    }

    # Run bootstrap with strata defined by the cluster variable
    results <- boot(data = data, statistic = boot_statistic, R = n_bootstraps, strata = data$grid, parallel = "multicore", ncpus = 4)
    results <- tidy(results)
    results <- results %>% select(term, statistic, std.error)
    results <- results %>% mutate(
        ci_lower = statistic - 1.96 * std.error,
        ci_upper = statistic + 1.96 * std.error,
        significant = ifelse(ci_lower > 0 | ci_upper < 0, "yes", "no"),
        ci = paste0('[', round(ci_lower, 1), ', ', round(ci_upper, 1), ']')
    )
    colnames(results) <- c("variable", paste0("estimate_", model_number), paste0("robust_se_", model_number), paste0("ci_", model_number), paste0("significant_", model_number))
    return(results)
}

## 5. Table 2: Descriptive Statistics

## 6. Table 3: Effects of the Seasonal Intensification of Fumigations on Birth Weight
table3_1 <- run_bootstrap(bananas1, "ols",
    dependent_var = "weight",
    independent_vars = c("bx"), control_vars = c("cohort", "grid"),
    n_bootstraps = 10, model_number = 1
)
table3_2 <- run_bootstrap(bananas1, "ols",
    dependent_var = "weight",
    independent_vars = c("bx * pxp"), control_vars = c("cohort", "grid"),
    n_bootstraps = 10, model_number = 2
)
table3_3 <- run_bootstrap(bananas1, "ols",
    dependent_var = "weight",
    independent_vars = c("bx * pxt1"), control_vars = c("cohort", "grid"),
    n_bootstraps = 10, model_number = 3
)
table3_4 <- run_bootstrap(bananas1, "ols",
    dependent_var = "weight",
    independent_vars = c("bx * pxt2"), control_vars = c("cohort", "grid"),
    n_bootstraps = 10, model_number = 4
)
table3_5 <- run_bootstrap(bananas1, "ols",
    dependent_var = "weight",
    independent_vars = c("bx * pxt3"), control_vars = c("cohort", "grid"),
    n_bootstraps = 10, model_number = 5
)
table3_6 <- run_bootstrap(bananas1, "ols",
    dependent_var = "weight",
    independent_vars = c("bx * pxt1", "bx * pxt2", "bx * pxt3"), control_vars = c("cohort", "grid"),
    n_bootstraps = 10, model_number = 6
)

table3_results <- full_join(table3_1, table3_2, by = "variable") %>%
    full_join(table3_3, by = "variable") %>%
    full_join(table3_4, by = "variable") %>%
    full_join(table3_5, by = "variable") %>%
    full_join(table3_6, by = "variable")
table3_results %>% print(n = 50)
table3_results <- table3_results[c(1, 13:20), ]
table3_results <- table3_results[c(1, 2, 3, 4, 6, 8, 5, 7, 9), ]

## 7. Table 4: Effects of the Seasonal Intensification of Fumigations on Gestation Weeks, Preterm, and Low Birth Weight
table4_1 <- run_bootstrap(bananas1, "ols",
    dependent_var = "gweeks",
    independent_vars = c("bx * pxp"), 
    control_vars = c("cohort", "grid"),
    cluster_vars = c('grid'),
    n_bootstraps = 10, model_number = 1
)

table4_2 <- run_bootstrap(bananas1, "ols",
    dependent_var = "gweeks",
    independent_vars = c("bx * pxt1", "bx * pxt2", "bx * pxt3"), control_vars = c("cohort", "grid"),
    n_bootstraps = 10, model_number = 2
)

table4_3 = run_bootstrap(bananas1, "logit",
    dependent_var = "preterm",
    independent_vars = c("bx * pxp"),
    fixed_effects = c("cohort", "grid"),
    n_bootstraps = 10, model_number = 3
)

table4_4 = run_bootstrap(bananas1, "logit",
    dependent_var = "preterm",
    independent_vars = c("bx * pxt1", "bx * pxt2", "bx * pxt3"), control_vars = c("cohort", "grid"),
    n_bootstraps = 10, model_number = 4
)

table4_5 = run_bootstrap(bananas1, "logit",
    dependent_var = "lbw",
    independent_vars = c("bx * pxp"), control_vars = c("cohort", "grid"),
    n_bootstraps = 10, model_number = 5
)

table4_6 = run_bootstrap(bananas1, "logit",
    dependent_var = "lbw",
    independent_vars = c("bx * pxt1", "bx * pxt2", "bx * pxt3"),
    control_vars = c("cohort", "grid"),
    cluster_vars = c("grid"),
    n_bootstraps = 10, model_number = 6
)

table4_results <- full_join(table4_1, table4_2, by = "variable") %>%
    full_join(table4_3, by = "variable") %>%
    full_join(table4_4, by = "variable") %>%
    full_join(table4_5, by = "variable") %>%
    full_join(table4_6, by = "variable")
table4_results %>% print(n = 50)
table4_results <- table4_results[c(1,2, 14:20), ]

## 8. Table 5: Effects of the Seasonal Intensification of Fumigations on Birth Weight, by Maternal Fixed Effects
eststo: reghdfe weight bx birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid grid)

#run_bootstrap <- function(data, model_type, dependent_var, independent_vars, control_vars, cluster_vars, n_bootstraps, model_number) {

table5_1 = run_bootstrap(
    data = bananas2, 
    model_type = "ols",
    dependent_var = "weight",
    independent_vars = c("bx",'birth_interval1','birth_interval2','birth_interval3'), 
    control_vars = c("cohort", "mfeid_grid"),
    cluster_vars = c("mfeid","grid"),
    n_bootstraps = 5, model_number = 1
)

