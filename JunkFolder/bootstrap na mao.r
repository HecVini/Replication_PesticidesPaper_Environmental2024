# 1. Indentify Clusters
# 2. Make a sample of the clusters (there will be repetitions)
# 3. Make a boostrap dataset using the sorted clusters. Notice that there will be duplicated rows
# 4. Get the coefs estimates and save it
# 5. Robust SE is given the s.d. of the saved coeficients

pb <- progress::progress_bar$new(
    format = "[:bar] :percent | ETA: :eta",
    total = 100,
    clear = FALSE,
    width = 60
)

options(max.print = 100)

bananas1['grid']
# 1. Indentify Clusters and make a sample of it
cluster_sample <- function(data, cluster) {
    cluster_list <- unique(data[[cluster]])
    sample_size <- length(cluster_list)
    cluster_sample <- sample(cluster_list, sample_size, replace = TRUE)
    
    return(cluster_sample)
}

df_teste <- data.frame(id = 1:100, cluste = rep(c("A", "B", "C", "D", "E"), 20), x = rnorm(100), y = rnorm(100), z = rnorm(100))

teste_sample <- cluster_sample(bananas2,'mfeid_grid') # Amostra dos clusters de size = 50x5 = 250
teste_sample

frequency_df <- as.data.frame(teste_sample)
frequency_df = tibble(frequency_df)
frequency_df = frequency_df %>% group_by(teste_sample) %>% count()
frequency_df = frequency_df %>% ungroup(teste_sample)

frequency_df = frequency_df %>% arrange(desc(n))
frequency_df 
sum(frequency_df$n)


# 2. Bootstrap dataset to find the coef and the robust SE
boot_dataset <- function(data, cluster) {
    cluster_sample <- cluster_sample(data,cluster) # sample with repetitions of clusters

    new_dataset <- data.frame() # Empty dataset
    for (i in 1:length(cluster_sample)) {
        temp_dataset <- data[cluster == cluster_sample[i], ]
        new_dataset <- bind_rows(new_dataset, temp_dataset) # Add the rows of the sample to the new dataset
    }
    return(new_dataset) # It will be a dataset with (nA + nB + nC + nD + nE) rows - if all equal, it will have 5*n_clusters rows
}

df_teste <- data.frame(id = 1:100, cluster = rep(c("A", "B", "C", "D", "E"), 20), x = rnorm(100), y = rnorm(100), z = rnorm(100))

teste2 <- boot_dataset(bananas2, 'mfeid_grid')
teste2

table(df_teste$id) # Conta quantas vezes cada cluster aparece - note que todos aparecem 20 vezes

# 3. Setup model
set_model <- function(data, dependent_var, independent_var, fixed_effects, model_type) {
    dependent_var_str <- dependent_var
    independent_var_str <- paste(independent_var, collapse = " + ")
    fixed_effects_str <- paste(fixed_effects, collapse = " + ")
    mom_controls <- paste(c("age", "educ", "private", "labors", "nchild", "visits", "nbl", "sex", "tlabor", "marital"), collapse = " + ") # Every rergression on the paper use these controls

    formula_str <- paste(dependent_var_str, "~", independent_var_str, "+", mom_controls, " | ", fixed_effects_str)
    formula <- as.formula(formula_str)

    if (model_type == "ols") {
        model <- felm(formula, data = data)
    } else if (model_type == "logit") {
        model <- feglm(formula, data = data, family = "logit")
    }
    return(model)
}

teste7 <- set_model(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c("bx", "ethnic"),
    fixed_effects = c("cohort", "grid"),
    model_type = "ols"
)
teste7

# 5. Get the coefs estimates and save it
first_estimation <- function(data, cluster, dependent_var, independent_var, fixed_effects, model_type) {
    boot_data <- boot_dataset(data, cluster) %>% tibble() # Make the boot dataset that will be used for one estiamtion

    boot_model <- set_model(boot_data, dependent_var, independent_var, fixed_effects, model_type) # Fit the model

    estimates <- tidy(boot_model)
    estimates <- estimates %>% subset(select = c(term, estimate)) # Clean results
    estimates <- as.data.frame(t(estimates))
    colnames(estimates) <- as.character(unlist(estimates[1, ])) # More janitoring
    estimates <- estimates[-1, ]
    estimates <- estimates %>%
        tibble() %>%
        clean_names() # Final df is a 1 x n_coeficients df

    estimates[] <- estimates[] %>% lapply(function(x) as.numeric(as.character(x))) # Convert all to numeric
    pb$tick() # Update the progress bar
    return(estimates)
}

teste8 <- first_estimation(
    data = bananas1,
    cluster = bananas1$grid,
    dependent_var = "weight",
    independent_var = c("bx", "ethnic"),
    fixed_effects = c("cohort", "grid"),
    model_type = "ols"
)
teste8

boot_estimation <- function(data, cluster, dependent_var, independent_var, fixed_effects, model_type, n_boot) {
    boot_estimates <- tibble()

    for (i in 1:n_boot) {
        get_coefs <- first_estimation(data, cluster, dependent_var, independent_var, fixed_effects, model_type)
        get_coefs$no_boot <- i
        boot_estimates <- bind_rows(boot_estimates, get_coefs)
        pb$tick() # Update the progress bar
    }
    return(boot_estimates)
}

teste5 <- boot_estimation(df_teste, df_teste$cluster, teste_formula, 10)

get_results <- function(data, cluster, dependent_var, independent_var, fixed_effects, model_type, n_boot) {
    boot_estimates <- boot_estimation(data, cluster, dependent_var, independent_var, fixed_effects, model_type, n_boot)
    print(boot_estimates)
    boot_estimates <- boot_estimates %>% subset(select = -c(no_boot)) # Remove the no_boot column

    boot_mean <- sapply(boot_estimates, mean, na.rm = TRUE)
    boot_mean <- as.data.frame(t(boot_mean))
    boot_mean <- boot_mean %>%
        tibble() %>%
        clean_names() # Final df is a 1 x n_coeficients df
    boot_mean[] <- boot_mean[] %>% lapply(function(x) as.numeric(as.character(x))) # Convert all to numeric
    boot_mean <- boot_mean %>% pivot_longer(cols = everything(), names_to = "coef", values_to = "mean")

    boot_se <- sapply(boot_estimates, sd, na.rm = TRUE)
    boot_se <- as.data.frame(t(boot_se))
    boot_se <- boot_se %>%
        tibble() %>%
        clean_names() # Final df is a 1 x n_coeficients df
    boot_se[] <- boot_se[] %>% lapply(function(x) as.numeric(as.character(x))) # Convert all to numeric
    boot_se <- boot_se %>% pivot_longer(cols = everything(), names_to = "coef", values_to = "robust_se")

    results <- full_join(boot_mean, boot_se, by = "coef")
    results <- results %>% mutate(ci_low = mean - 1.96 * robust_se, ci_high = mean + 1.96 * robust_se)
    results <- results %>% mutate(significant = ifelse(ci_low > 0 & ci_high > 0, "yes", "no"))
    results <- results %>% mutate(ci = paste0("[", round(ci_low, 2), ", ", round(ci_high, 2), "]"))
    results <- results %>% select(coef, mean, robust_se, ci, significant)

    return(results)
}

teste6 <- get_results(df_teste, df_teste$cluster, teste_formula, 10)
teste6

x <- "-----------------------------------------------------------------------------------------------------------------------"
set_model <- function(data, dependent_var, independent_var, fixed_effects, model_type) {
    dependent_var_str <- dependent_var
    independent_var_str <- paste(independent_var, collapse = " + ")
    fixed_effects_str <- paste(fixed_effects, collapse = " + ")
    mom_controls <- paste(c("age", "educ", "private", "labors", "nchild", "visits", "nbl", "sex", "tlabor", "marital"), collapse = " + ") # Every rergression on the paper use these controls

    formula_str <- paste(dependent_var_str, "~", independent_var_str, "+", mom_controls, " | ", fixed_effects_str)
    formula <- as.formula(formula_str)

    if (model_type == "ols") {
        model <- felm(formula, data = data)
    } else if (model_type == "logit") {
        model <- feglm(formula, data = data, family = "logit")
    }
    return(model)
}

x = set_model(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c('bx','ethnic'),
    fixed_effects = c('cohort','grid'),
    model_type = "ols")

boot_function <- function(data, indices, dependent_var, independent_var, fixed_effects, model_type, cluster_var) {
    data <- data[indices, ]

    clusters_list <- unique(data[[cluster_var]])
    sample_size <- length(clusters_list) # Sample size = number of unique clusters (but the sample will have repetitions and non-apperences)

    cluster_sample <- sample(x = clusters_list, size = sample_size, replace = TRUE) # Sample of the clusters with repetition
    print(table(cluster_sample))

    boot_sample <- data.frame() # Empty dataset
    # Get the rows corresponding to the sampled clusters
    for (i in 1:length(cluster_sample)) {
        cluster_rows <- data %>% filter(.[[cluster_var]] == cluster_sample[i])
        print(c('Iteration',i,'with cluster =',cluster_sample[i]))

        boot_sample <- rbind(boot_sample, cluster_rows)

    }
    print(boot_sample %>% tibble())

    # Fit the model and return coefficients (modify this according to your model)
    model <- set_model(boot_sample, dependent_var, independent_var, fixed_effects, model_type)
    return(coef(model))
}

boot_results <- boot(
    data = bananas1, 
    statistic = boot_function, 
    R = 10,
    parallel = "multicore", 
    ncpus = 4,
    # Pass the additional arguments directly
    dependent_var = "weight",
    independent_var = c("bx", "ethnic"),
    fixed_effects = c("cohort", "grid"),
    model_type = "ols",
    cluster_var = "grid"
)
boot_results

boot_results2 <- boot(
    data = bananas1, 
    statistic = boot_function, 
    R = 10,
    parallel = "multicore", 
    ncpus = 4,
    # Pass the additional arguments directly
    dependent_var = "weight",
    independent_var = c("bx", "ethnic"),
    fixed_effects = c("cohort", "grid"),
    model_type = "ols",
    cluster_var = "grid"
)

tidy(boot_results)

x <- "-----------------------------------------------------------------------------------------------------------------------"

# 1. Indentify Clusters and make a sample of it
cluster_sample <- function(data,cluster_var) { # 'cluster' should be a vector of the clusters (or data$cluster)
    cluster_list <- unique(data[[cluster_var]])
    sample_size <- length(cluster_list) # Sample size = number of unique clusters (but the sample will have repetitions and non-apperences)

    cluster_sample <- sample(cluster_list, sample_size, replace = TRUE) # Sample of the clusters with
    return(cluster_sample)
}

df_teste = data.frame(id = 1:100, cluster = rep(c("A", "B", "C", "D", "E"), 20), x = rnorm(100), y = rnorm(100), z = rnorm(100))
teste1 = cluster_sample(df_teste,'cluster') #ok

teste1 = cluster_sample(bananas2,'mfeid_grid')


# 2. Bootstrap dataset to find the coef and the robust SE
boot_dataset <- function(data, cluster_var) {
    cluster_sample <- cluster_sample(data,cluster_var) # sample with repetitions of clusters
    print(table(cluster_sample))

    boot_dataset <- data.frame() # Empty dataset
    for (i in 1:length(cluster_sample)) {
        temp_dataset <- data %>% filter(.[[cluster_var]] == cluster_sample[i]) #temp_dataset = dataset with rows for cluster_sample[i], there will be repetitions and non-apperences
        boot_dataset <- bind_rows(boot_dataset, temp_dataset) # Add the rows of the sample to the new dataset
    }
    return(boot_dataset) # It will be a dataset with different size from data. Since some clusters appear more than once, this will happern
}

teste2 <- boot_dataset(df_teste, 'cluster')
teste2 #ok

# 3. Setup model
set_model <- function(data, dependent_var, independent_var, fixed_effects, model_type) {
    dependent_var_str <- dependent_var
    independent_var_str <- paste(independent_var, collapse = " + ")
    fixed_effects_str <- paste(fixed_effects, collapse = " + ")
    mom_controls <- paste(c("age", "educ", "private", "labors", "nchild", "visits", "nbl", "sex", "tlabor", "marital"), collapse = " + ") # Every rergression on the paper use these controls

    formula_str <- paste(dependent_var_str, "~", independent_var_str, "+", mom_controls, " | ", fixed_effects_str)
    formula <- as.formula(formula_str)
    print("MODEL FORMULA BELOW:")
    print(formula)

    if (model_type == "ols") {
        model <- felm(formula, data = data)
    } else if (model_type == "logit") {
        model <- feglm(formula, data = data, family = "logit")
    }
    return(model)
}

teste3 <- set_model(
    data = bananas1,
    dependent_var = "preterm",
    independent_var = c("bx", "pxp", "bx*pxp","ethnic"),
    fixed_effects = c("cohort", "grid"),
    model_type = "logit"
)
teste3 #ok, it returns the estimates for the coeficients
tidy(teste3)

# 4. Get the coefs estimates and save it
estimate_coefs <- function(data, dependent_var, independent_var, fixed_effects, cluster_var, model_type) {
    boot_data <- boot_dataset(data, cluster_var) %>% tibble() # Make the boot dataset that will be used for one estiamtion
    boot_model <- set_model(boot_data, dependent_var, independent_var, fixed_effects, model_type) # Fit the model

    estimates <- tidy(boot_model)
    estimates <- estimates %>% subset(select = c(term, estimate)) # Clean results
    estimates <- as.data.frame(t(estimates))
    colnames(estimates) <- as.character(unlist(estimates[1, ])) # More janitoring
    estimates <- estimates[-1, ]
    estimates <- estimates %>%
        tibble() %>%
        clean_names() # Final df is a 1 x n_coeficients df

    estimates[] <- estimates[] %>% lapply(function(x) as.numeric(as.character(x))) # Convert all to numeric
    return(estimates)
}

teste4 <- estimate_coefs(
    data = bananas1,
    dependent_var = "preterm",
    independent_var = c("bx", "pxp", "bx*pxp"),
    fixed_effects = c("cohort", "grid"),
    cluster_var = "grid",
    model_type = "logit"
)
teste4 #ok, it returns the estimates for the coeficients using just one sample

teste6 = estimate_coefs(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c("bx"),
    fixed_effects = c("cohort", "grid"),
    cluster_var = "grid",
    model_type = "ols"
)
teste6 = felm(weight ~ bx + age + educ + private + labors + nchild + visits + n)

# 5. Bootstrap once
run_bootstrap <- function(data, dependent_var, independent_var, fixed_effects, cluster_var, model_type, n_bootstraps) {
    boot_estimates <- tibble()

    for (i in 1:n_bootstraps) {
        coefs_boot_i <- estimate_coefs(data, dependent_var, independent_var, fixed_effects, cluster_var, model_type)
        coefs_boot_i$no_boot <- i
        boot_estimates <- bind_rows(boot_estimates, coefs_boot_i)
    }
    return(boot_estimates)
}

teste5 <- run_bootstrap(
    data = bananas1,
    dependent_var = "preterm",
    independent_var = c("bx", "pxp", "bx*pxp"),
    fixed_effects = c("cohort", "grid"),
    cluster_var = "grid",
    model_type = "logit",
    n_bootstraps = 10)
teste5 #ok, it returns the estimates for the coeficients using 10 samples

# 6. Bootstrap multiple times and get the results
get_results <- function(data, dependent_var, independent_var, fixed_effects, model_type, cluster_var, n_bootstraps) {
    boot_estimates <- run_bootstrap(data, dependent_var, independent_var, fixed_effects, cluster_var, model_type, n_bootstraps)
    print(head(boot_estimates))

    boot_estimates <- boot_estimates %>% subset(select = -c(no_boot)) # Remove the no_boot column

    if(model_type == "logit"){
        boot_estimates[] = exp(boot_estimates[])
    } else if(model_type == "ols"){
        boot_estimates = boot_estimates 
    }


    boot_mean <- sapply(boot_estimates, mean, na.rm = TRUE)
    boot_mean <- as.data.frame(t(boot_mean))
    boot_mean <- boot_mean %>%
        tibble() %>%
        clean_names() # Final df is a 1 x n_coeficients df
    boot_mean[] <- boot_mean[] %>% lapply(function(x) as.numeric(as.character(x))) # Convert all to numeric
    boot_mean <- boot_mean %>% pivot_longer(cols = everything(), names_to = "coef", values_to = "mean")

    boot_se <- sapply(boot_estimates, sd, na.rm = TRUE)
    boot_se <- as.data.frame(t(boot_se))
    boot_se <- boot_se %>%
        tibble() %>%
        clean_names() # Final df is a 1 x n_coeficients df
    boot_se[] <- boot_se[] %>% lapply(function(x) as.numeric(as.character(x))) # Convert all to numeric
    boot_se <- boot_se %>% pivot_longer(cols = everything(), names_to = "coef", values_to = "robust_se")

    results <- full_join(boot_mean, boot_se, by = "coef")
    results <- results %>% mutate(ci_low = mean - 1.96 * robust_se, ci_high = mean + 1.96 * robust_se)

    if(model_type == "logit"){
        results <- results %>% mutate(significant = ifelse(ci_low > 1 & ci_high > 1, "yes", "no"))
    } else if(model_type == "ols"){
        results <- results %>% mutate(significant = ifelse(ci_low > 0 & ci_high > 0, "yes", "no"))
    }
    
    results <- results %>% mutate(ci = paste0("[", round(ci_low, 2), ", ", round(ci_high, 2), "]"))
    results <- results %>% select(coef, mean, robust_se, ci, significant)

    return(results)
}

birth_interval = c("birth_interval1","birth_interval2","birth_interval3")
teste6 <- get_results(
    data = bananas1,
    dependent_var = "weight",
    independent_var = c("bx"),
    fixed_effects = c("cohort", "grid"),
    cluster_var = "grid",
    model_type = "ols",
    n_bootstraps = 20)
teste6





