## 1. Load Packages
library(tidyverse) # Easily Installand Load the 'Tidyverse' 
library(lubridate) # Make Dealing with Dates a Little Easier 
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data 
library(data.table) # Extension of `data.frame`
library(tidylog) # Logging for 'dplyr' and 'tidyr' Functions 
library(haven) #Open .xpt and Stata (.do) files
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
library(lmtest)   # For coefficient testing with robust standard errors

# linter:disable
lint(
  text = strrep("x", 400L),
  linters = line_length_linter(length = 400L)
)

lint()


## 2. Load Data
setwd("C:/Users/vinic/OneDrive/Mestrado/5Tri/EnvironmentalUrban/ReplicationPackage")
...
bananas1.R = read_dta("bananas1.dta") #Main dataset used on the papaer
bananas2.R = read_dta("bananas2.dta") #Dataset used to generate the tables with maternal fixed effects
bananas3.R = read_dta("bananas3.dta") #Dataset used to generate the tables that examine the effects of pesticides in other crop plantations

## 3. Get the variables labels
bananas1 = bananas1.R %>% clean_names() #Clean the column names
bananas1$marital <- as.factor(bananas1$marital)
bananas1$ethnic <- as.factor(bananas1$ethnic)
bananas1$cohort <- as.factor(bananas1$cohort)
bananas1$grid <- as.factor(bananas1$grid)

bananas2 = bananas2.R %>% clean_names() #Clean the column names
bananas3 = bananas3.R %>% clean_names() #Clean the column names

GetBananasVariableDescription = function(dataset){
    dataset_length = dim(dataset)[2] #Get the number of rows
    dataset_labels = sapply(bananas1, function(column) attr(column, "label")) #Get the labels of the columns (its a messy list)
    dataset_types = sapply(bananas1, function(column) attr(column, "format.stata"))
    dataset_variables_description = tibble(column_name = NA, label = NA, format = NA) #Create a tibble to store the variables description
    for(i in c(1:dataset_length)){
    dataset_variables_description[i,1] = names(dataset[i])
    dataset_variables_description[i,2] = dataset_labels[[i]] 
    dataset_variables_description[i,3] = dataset_types[[i]]
    } #complete the tibble with the variables description. First column: each name, second column: each label
    return(dataset_variables_description)
    }

bananas1_variables_description = GetBananasVariableDescription(bananas1)
bananas1_variables_description %>% print()
bananas2_variables_description = GetBananasVariableDescription(bananas2)
bananas2_variables_description
bananas3_variables_description = GetBananasVariableDescription(bananas3) #Tem problema aqui, depois vejo

## 4. Make Table 2
columns_table2 = c(
    "weight", "gweeks", "lbw", "preterm", "age", "dsex1", "deduc1", 
    "dethnic6", "tlabor1", "dmarital1", "dmarital2", "dmarital3", 
    "dprivate1", "labors", "nchild", "visits", "dnbl1", "esample", "bx","pxp"
)

colwise_function = function(data,func,group){
    func_name = deparse(substitute(func)) #Get the name of the function
    results = apply(data, MARGIN = 2, FUN = func, na.rm = TRUE)
    results = as.list(results) # Transform the means into a list
    results = tibble(column_name = names(results), mean = unlist(results)) 
    colnames(results) = c("column_name", paste0(func_name,"_",group))
    return(results)
} # Generic function to apply a function to each column of a dataframe

colwise_merge = function(data,group){
    data1 = colwise_function(data,mean,group)
    data2 = colwise_function(data,sd,group)

    merged_data = full_join(data1,data2,by = 'column_name')
    return(merged_data)
} # Generic function to merge two dataframes by the column_name

table2 = bananas1 %>% subset(select = columns_table2)# Subset data to get the variables used in table 2

table2_1 = table2 %>% filter(esample == 1) #esample: newborn present in final sample (just 21 observations have newsample != 1)
table2_1 = table2_1 %>% select(-c(esample,bx,pxp)) #Remove the esample column
table_2_1_observations = dim(table2_1)[1] #Get the number of observations
table2_1_results = colwise_merge(table2_1,1) #Get means and std for this group

table2_2 = table2 %>% filter(esample == 1, bx == 0) #bx = dummy for exposure to pesticides (1 = yes)
table2_2 = table2_2 %>% select(-c(esample,bx,pxp)) #Remove the esample and bx columns
table_2_2_observations = dim(table2_2)[1] #Get the number of observations
table2_2_results = colwise_merge(table2_2,2)

table2_3 = table2 %>% filter(esample == 1, bx == 1)
table2_3 = table2_3 %>% select(select = -c(esample,bx,pxp)) #Remove the esample column
table_2_3_observations = dim(table2_3)[1] #Get the number of observations
table2_3_results = colwise_merge(table2_3,3) #Get means and std for this group

table2_4 = table2 %>% filter(esample == 1, pxp == 0) #pxp: exposure to pesticides 
table2_4 = table2_4 %>% select(select = -c(esample,bx,pxp)) #Remove the esample column
table_2_4_observations = dim(table2_4)[1] #Get the number of observations
table2_4_results = colwise_merge(table2_4,4) #Get means and std for this group

table2_5 = table2 %>% filter(esample == 1, pxp == 1) #pxp: exposure to pesticides 
table2_5 = table2_5 %>% select(select = -c(esample,bx,pxp)) #Remove the esample column
table_2_5_observations = dim(table2_5)[1] #Get the number of observations
table2_5_results = colwise_merge(table2_5,5) #Get means and std for this group

table2_results = full_join(table2_1_results,table2_2_results,by = 'column_name') #Merge the results
table2_results = full_join(table2_results,table2_3_results,by = 'column_name') #Merge the results
table2_results = full_join(table2_results,table2_4_results,by = 'column_name') #Merge the results
table2_results = full_join(table2_results,table2_5_results,by = 'column_name') #Merge the results

table2_replicated = gt(table2_results)
table2_replicated = table2_replicated %>% tab_header(
    title = "Table 2: Descriptive Statistics for the Full Sample and by Exposure to Pesticides")
table2_replicated = table2_replicated %>% 
    fmt_number(columns = c(mean_1,mean_2,mean_3,mean_4,mean_5), decimals = 1) %>%
    fmt_number(columns = c(sd_1,sd_2,sd_3,sd_4,sd_5), decimals = 1)
table2_replicated = table2_replicated %>% cols_label(
    column_name = "", 
    mean_1 = "Mean", 
    sd_1 = "Std. Deviation",
    mean_2 = "Mean",
    sd_2 = "Std. Deviation",
    mean_3 = "Mean",
    sd_3 = "Std. Deviation",
    mean_4 = "Mean",
    sd_4 = "Std. Deviation",
    mean_5 = "Mean",
    sd_5 = "Std. Deviation"
)
table2_replicated = table2_replicated %>% 
    tab_spanner(
        label = "All Newborns", columns = c(mean_1,sd_1)) %>%
    tab_spanner(
        label = "Not Exposed to Banana Plantations", columns = c(mean_2,sd_2)) %>%
    tab_spanner(
        label = "Exposed to Banana Plantations", columns = c(mean_3,sd_3)) %>%
    tab_spanner(
        label = "Not Exposed to Pesticides", columns = c(mean_4,sd_4)) %>%
    tab_spanner(
        label = "Exposed to Pesticides", columns = c(mean_5,sd_5))
table2_replicated = table2_replicated %>% tab_options(
    heading.background.color = "#ffffff",
    column_labels.font.size = px(12),
    data_row.padding = px(5),
    row_group.background.color = "white",
    row_group.border.bottom.color = "white",
    table.border.color = "white")

## 5. Make Table 3
table3_controls = c("age", "educ", "private", "labors", "nchild", "visits", "nbl", "sex", "tlabor", "marital", "ethnic")
table3_columns = c('bx','pxp','pxt1','pxt2','pxt3',table3_controls)
table3_columns = c('weight',table3_columns)
table3_columns = c('grid','cohort',table3_columns)

table3 = bananas1 %>% subset(select = table3_columns) # Subset data to get the variables used in table 3
table3$marital <- as.factor(table3$marital)
table3$ethnic <- as.factor(table3$ethnic)

# Define the regression model function
ols_model = function(data, indices, interest_independent_var, interesting_dependent_var) {
    d <- data[indices,] # Allows bootstrapping to sample the data
    formula <- as.formula(paste(interesting_dependent_var, "~", interest_independent_var, "+ age + educ + private + labors + nchild + visits + nbl + sex + tlabor + marital + ethnic | cohort + grid"))
    fit <- felm(formula, data = d)
    return(coef(fit))
} #Set the model to be used in the bootstrap

run_bootstrap = function(data, interest_independent_var, interesting_dependent_var, rep, model_number) {
    boot_statistic <- function(d, i) {
        ols_model(d, i, interest_independent_var, interesting_dependent_var) # Calls the function to estimate the model
    }
    results <- boot(data = data, statistic = boot_statistic, R = rep, parallel = "multicore", ncpus = 4) # Run the bootstrap
    results = tidy(results, conf.int = TRUE, conf.method = "perc") # Get the results in a tidy format
    results = results %>% subset(select = c(term,statistic,std.error))
    colnames(results) = c("term",paste0("mean_",model_number),paste0("se_",model_number))
    return(results) #Notice that std changes according to seed, which changes the individual bootstraps - it will not be the same (bot the mean is very close. that LLN)
} # It uses set_model() to run the bootstrap

table3_1 = run_bootstrap(table3, "bx", "weight", rep = 10, model_number = 1) #Bootstrap for the first model
table3_1 = run_bootstrap(table3, "bx", "weight", rep = 10, model_type = 'ols', model_number = 1)
table3_2 = run_bootstrap(table3, "bx * pxp", "weight", rep = 10, model_number = 2) 
table3_3 = run_bootstrap(table3, "bx * pxt1", "weight", rep = 10, model_number = 3)
table3_4 = run_bootstrap(table3, "bx * pxt2", "weight", rep = 10, model_number = 4)
table3_5 = run_bootstrap(table3, "bx * pxt3", "weight", rep = 10, model_number = 5) # nolint
table3_6 = run_bootstrap(table3, "bx * pxt1 + bx * pxt2 + bx * pxt3", "weight", rep = 10, model_number = 6)

table3_results = full_join(table3_1,table3_2,by = 'term') #Merge the results
table3_results = full_join(table3_results,table3_3,by = 'term') #Merge the results
table3_results = full_join(table3_results,table3_4,by = 'term') #Merge the results
table3_results = full_join(table3_results,table3_5,by = 'term') #Merge the results
table3_results = full_join(table3_results,table3_6,by = 'term') #Merge the results

table3_results = table3_results[c(1,24,25,26,28,30,27,29,31),] #Get the rows that are interesting

## 6. Make Table 4
setup_model = function(data, indices, interest_independent_var, interesting_dependent_var, model_type) {
    d <- data[indices,] # Allows bootstrapping to sample the data
    
    if (model_type == "ols") {
        formula <- as.formula(paste(interesting_dependent_var, "~", interest_independent_var, "+ age + educ + private + labors + nchild + visits + nbl + sex + tlabor + marital + ethnic | cohort + grid"))
        fit <- felm(formula, data = d)
    } else if (model_type == 'logit') {
        formula <- as.formula(paste(interesting_dependent_var, "~", interest_independent_var, "+ age + educ + private + labors + nchild + visits + nbl + sex + tlabor + marital + ethnic"))
        fit <- glm(formula, data = d, family = binomial())
    }
    return(coef(fit))
}

run_bootstrap = function(data, interest_independent_var, interesting_dependent_var, rep, model_type, model_number) {
    boot_statistic <- function(d, i) {
        coef(setup_model(d, i, interest_independent_var, interesting_dependent_var, model_type))
    }
    results <- boot(data = data, statistic = boot_statistic, R = rep, parallel = "multicore", ncpus = 4) # Run the bootstrap
    tidy_results <- tidy(results, conf.int = TRUE, conf.method = "perc") # Get the results in a tidy format
    selected_results <- select(tidy_results, term, statistic, std.error) # Selecting required columns
    colnames(selected_results) <- c("term", paste0("mean_", model_number), paste0("se_", model_number))
    return(selected_results)
}


## 6. Make Table 4
setup_model = function(data, indices, model_type, ind_var, exp_var) {
    d <- data[indices,] # Allows bootstrapping to sample the data

    # For logistic regression, ensure 'cohort' and 'grid' are factors
    if (model_type == 'logit') {
        d$cohort <- as.factor(d$cohort)
        d$grid <- as.factor(d$grid)
    }

    # Define the model formula
    if (model_type == "ols") {
        formula <- as.formula(paste(ind_var, "~", exp_var, "+ age + educ + private + labors + nchild + visits + nbl + sex + tlabor + marital + ethnic | cohort + grid"))
        fit <- felm(formula, data = d)
    } else if (model_type == 'logit') {
        formula <- as.formula(paste(ind_var, "~", exp_var, "+ age + educ + private + labors + nchild + visits + nbl + sex + tlabor + marital + ethnic + cohort + grid"))
        fit <- glm(formula, data = d, family = binomial())
    }

    return(coef(fit))
}


setup_model(table3, 1:nrow(table3), "logit", "preterm", "bx")


## 8. Make Table 6


