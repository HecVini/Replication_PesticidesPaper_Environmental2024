## 1. Load Packages
library(tidyverse) # Easily Installand Load the 'Tidyverse' 
library(lubridate) # Make Dealing with Dates a Little Easier 
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data 
library(data.table) # Extension of `data.frame`
library(tidylog) # Logging for 'dplyr' and 'tidyr' Functions 
library(haven) #Open .xpt and Stata (.do) files
library(foreign) # Read and Write Data from Other Statistical Systems
library(Hmisc) # Harrell Miscellaneous. Used to extract .dta file label easily
library(htmltools)

## 2. Load Data
setwd("C:/Users/vinic/OneDrive/Mestrado/5Tri/EnvironmentalUrban/ReplicationPackage")
bananas1.R = read_dta("bananas1.dta") #Main dataset used on the papaer
bananas2.R = read_dta("bananas2.dta") #Dataset used to generate the tables with maternal fixed effects
bananas3.R = read_dta("bananas3.dta") #Dataset used to generate the tables that examine the effects of pesticides in other crop plantations

## 3. Get the variables labels
bananas1 = bananas1.R %>% clean_names() #Clean the column names
bananas2 = bananas2.R %>% clean_names() #Clean the column names
bananas3 = bananas3.R %>% clean_names() #Clean the column names
bananas3

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
bananas2_variables_description = GetBananasVariableDescription(bananas2)
bananas3_variables_description = GetBananasVariableDescription(bananas3) #Tem problema aqui, depois vejo

## 4. Make Table 2

## 5. Make Table 3

## 6. Make Table 4

## 7. Make Table 5

## 8. Make Table 6
