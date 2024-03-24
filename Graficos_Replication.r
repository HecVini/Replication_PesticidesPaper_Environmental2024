## 1. Load Packages
library(tidyverse) # Easily Installand Load the 'Tidyverse'
library(lubridate) # Make Dealing with Dates a Little Easier
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(data.table) # Extension of `data.frame`
#library(tidylog) # Logging for 'dplyr' and 'tidyr' Functions
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
library(fixest)

## 2. Load Data
setwd("C:/Users/vinic/OneDrive/Mestrado/5Tri/EnvironmentalUrban/ReplicationPackage")
tom_azul = '#434371'

table3_reg1 = read.csv("regressions_results/table3_reg1.csv") %>% tibble() %>% subset(select = -1)
table3_reg2 = read.csv("regressions_results/table3_reg2.csv") %>% tibble() %>% subset(select = -1)
table3_reg3 = read.csv("regressions_results/table3_reg3.csv") %>% tibble() %>% subset(select = -1)
table3_reg4 = read.csv("regressions_results/table3_reg4.csv") %>% tibble() %>% subset(select = -1)
table3_reg5 = read.csv("regressions_results/table3_reg5.csv") %>% tibble() %>% subset(select = -1)
table3_reg6 = read.csv("regressions_results/table3_reg6.csv") %>% tibble() %>% subset(select = -1)

table4_reg1 = read.csv("regressions_results/table4_reg1.csv") %>% tibble() %>% subset(select = -1)
table4_reg2 = read.csv("regressions_results/table4_reg2.csv") %>% tibble() %>% subset(select = -1)
table4_reg3 = read.csv("regressions_results/table4_reg3.csv") %>% tibble() %>% subset(select = -1)
table4_reg4 = read.csv("regressions_results/table4_reg4.csv") %>% tibble() %>% subset(select = -1)
table4_reg5 = read.csv("regressions_results/table4_reg5.csv") %>% tibble() %>% subset(select = -1)
table4_reg6 = read.csv("regressions_results/table4_reg6.csv") %>% tibble() %>% subset(select = -1)

table5_reg1 = read.csv("regressions_results/table5_reg1.csv") %>% tibble() %>% subset(select = -1)
table5_reg2 = read.csv("regressions_results/table5_reg2.csv") %>% tibble() %>% subset(select = -1)
table5_reg3 = read.csv("regressions_results/table5_reg3.csv") %>% tibble() %>% subset(select = -1)
table5_reg4 = read.csv("regressions_results/table5_reg4.csv") %>% tibble() %>% subset(select = -1)
table5_reg5 = read.csv("regressions_results/table5_reg5.csv") %>% tibble() %>% subset(select = -1)
table5_reg6 = read.csv("regressions_results/table5_reg6.csv") %>% tibble() %>% subset(select = -1)
table5_reg7 = read.csv("regressions_results/table5_reg7.csv") %>% tibble() %>% subset(select = -1)
table5_reg8 = read.csv("regressions_results/table5_reg8.csv") %>% tibble() %>% subset(select = -1)

table6_reg1 = read.csv("regressions_results/table6_reg1.csv") %>% tibble() %>% subset(select = -1)
table6_reg2 = read.csv("regressions_results/table6_reg2.csv") %>% tibble() %>% subset(select = -1)
table6_reg3 = read.csv("regressions_results/table6_reg3.csv") %>% tibble() %>% subset(select = -1)
table6_reg4 = read.csv("regressions_results/table6_reg4.csv") %>% tibble() %>% subset(select = -1)
table6_reg5 = read.csv("regressions_results/table6_reg5.csv") %>% tibble() %>% subset(select = -1)
table6_reg6 = read.csv("regressions_results/table6_reg6.csv") %>% tibble() %>% subset(select = -1)
table6_reg7 = read.csv("regressions_results/table6_reg7.csv") %>% tibble() %>% subset(select = -1)

## 3. Clean Data
table3 = full_join(table3_reg1, table3_reg2, by = "coef") %>%
  full_join(table3_reg3, by = "coef") %>%
  full_join(table3_reg4, by = "coef") %>%
  full_join(table3_reg5, by = "coef") %>%
  full_join(table3_reg6, by = "coef")
table3_reg3
table3
