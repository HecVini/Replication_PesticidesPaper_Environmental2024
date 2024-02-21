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

bananas1_ColumnNames <- names(bananas1) #Get the column names
bananas1_ColumnLabels <- sapply(bananas1, function(column) attr(column, "label")) #Get the column labels

bananas1_VariablesDescription <- data.frame(colnames = col_names, labels = col_labels,stringsAsFactors = FALSE) #Create a dataframe with the column names and labels
bananas1_VariablesDescription = tibble(bananas1_VariablesDescription$colnames,bananas1_VariablesDescription$labels) #Transform the dataframe into a tibble
colnames(bananas1_VariablesDescription) = c("variable","label") #Rename the columns

bananas2_ColumnNames <- names(bananas2) #Get the column names
bananas2_ColumnLabels <- sapply(bananas2, function(column) attr(column, "label")) #Get the column labels
bananas2_VariablesDescription <- data.frame(colnames = col_names, labels = col_labels,stringsAsFactors = FALSE) #Create a dataframe with the column names and labels
bananas2_VariablesDescription = tibble(bananas2_VariablesDescription$colnames,bananas2_VariablesDescription$labels) #Transform the dataframe into a tibble
colnames(bananas2_VariablesDescription) = c("variable","label") #Rename the columns

bananas3_ColumnNames <- names(bananas3) #Get the column names
bananas3_ColumnLabels <- sapply(bananas3, function(column) attr(column, "label")) #Get the column labels
bananas3_VariablesDescription <- data.frame(colnames = col_names, labels = col_labels,stringsAsFactors = FALSE) #Create a dataframe with the column names and labels
bananas3_VariablesDescription = tibble(bananas3_VariablesDescription$colnames,bananas3_VariablesDescription$labels) #Transform the dataframe into a tibble
colnames(bananas3_VariablesDescription) = c("variable","label") #Rename the columns

bananas1_VariablesDescription %>% filter(variable == "esample") #Check the variable description 
bananas1_VariablesDescription
bananas2_VariablesDescription
bananas3_VariablesDescription

colnames(bananas1) %>% sort() #Check the column names

## 4. Remake bananas.do on R
### 4.1. Table 2
UsedVariables = c("weight","gweeks","lbw","preterm","age","dsex1","deduc1","dethnic6","tlabor1","dmarital1","dmarital2","dmarital3","dprivate1","labors","nchild","visits","dnbl1","esample")
table2 = bananas1 %>% subset(select = UsedVariables) #Subset the variables

table2_AllNewborns <- table2 %>% filter(esample == 1) #Subset for only the babies on the sample (what esample == 0 means?) 
table2_AllNewborns_Results <- sapply(table2_AllNewborns[UsedVariables], mean, na.rm = TRUE) #Calculate the mean for each variable
table2_AllNewborns_Results = as.data.frame(table2_AllNewborns_Results) #Transform the results into a dataframe (it was a list)
table2_AllNewborns_Results = table2_AllNewborns_Results$table2_AllNewborns_Results #Put the results into a list
table2_AllNewborns_Results = tibble(UsedVariables,table2_AllNewborns_Results) #Make a 2 columns dataframe out of the names and results
colnames(table2_AllNewborns_Results) = c('variable','mean') #Rename the columns
table2_AllNewborns_Results 
