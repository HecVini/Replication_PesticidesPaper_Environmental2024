## 1. Load Packages
library(tidyverse) # Easily Installand Load the 'Tidyverse'
library(lubridate) # Make Dealing with Dates a Little Easier
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(data.table) # Extension of `data.frame`
# library(tidylog) # Logging for 'dplyr' and 'tidyr' Functions
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
tom_azul <- "#434371"

table3_reg1 <- read.csv("regressions_results/table3_reg1.csv") %>%
  tibble() %>%
  subset(select = -1)
table3_reg2 <- read.csv("regressions_results/table3_reg2.csv") %>%
  tibble() %>%
  subset(select = -1)
table3_reg3 <- read.csv("regressions_results/table3_reg3.csv") %>%
  tibble() %>%
  subset(select = -1)
table3_reg4 <- read.csv("regressions_results/table3_reg4.csv") %>%
  tibble() %>%
  subset(select = -1)
table3_reg5 <- read.csv("regressions_results/table3_reg5.csv") %>%
  tibble() %>%
  subset(select = -1)
table3_reg6 <- read.csv("regressions_results/table3_reg6.csv") %>%
  tibble() %>%
  subset(select = -1)

table4_reg1 <- read.csv("regressions_results/table4_reg1.csv") %>%
  tibble() %>%
  subset(select = -1)
table4_reg2 <- read.csv("regressions_results/table4_reg2.csv") %>%
  tibble() %>%
  subset(select = -1)
table4_reg3 <- read.csv("regressions_results/table4_reg3.csv") %>%
  tibble() %>%
  subset(select = -1)
table4_reg4 <- read.csv("regressions_results/table4_reg4.csv") %>%
  tibble() %>%
  subset(select = -1)
table4_reg5 <- read.csv("regressions_results/table4_reg5.csv") %>%
  tibble() %>%
  subset(select = -1)
table4_reg6 <- read.csv("regressions_results/table4_reg6.csv") %>%
  tibble() %>%
  subset(select = -1)

table5_reg1 <- read.csv("regressions_results/table5_reg1.csv") %>%
  tibble() %>%
  subset(select = -1)
table5_reg2 <- read.csv("regressions_results/table5_reg2.csv") %>%
  tibble() %>%
  subset(select = -1)
table5_reg3 <- read.csv("regressions_results/table5_reg3.csv") %>%
  tibble() %>%
  subset(select = -1)
table5_reg4 <- read.csv("regressions_results/table5_reg4.csv") %>%
  tibble() %>%
  subset(select = -1)
table5_reg5 <- read.csv("regressions_results/table5_reg5.csv") %>%
  tibble() %>%
  subset(select = -1)
table5_reg6 <- read.csv("regressions_results/table5_reg6.csv") %>%
  tibble() %>%
  subset(select = -1)
table5_reg7 <- read.csv("regressions_results/table5_reg7.csv") %>%
  tibble() %>%
  subset(select = -1)
table5_reg8 <- read.csv("regressions_results/table5_reg8.csv") %>%
  tibble() %>%
  subset(select = -1)

table6_reg1 <- read.csv("regressions_results/table6_reg1.csv") %>%
  tibble() %>%
  subset(select = -1)
table6_reg2 <- read.csv("regressions_results/table6_reg2.csv") %>%
  tibble() %>%
  subset(select = -1)
table6_reg3 <- read.csv("regressions_results/table6_reg3.csv") %>%
  tibble() %>%
  subset(select = -1)
table6_reg4 <- read.csv("regressions_results/table6_reg4.csv") %>%
  tibble() %>%
  subset(select = -1)
table6_reg5 <- read.csv("regressions_results/table6_reg5.csv") %>%
  tibble() %>%
  subset(select = -1)
table6_reg6 <- read.csv("regressions_results/table6_reg6.csv") %>%
  tibble() %>%
  subset(select = -1)
table6_reg7 <- read.csv("regressions_results/table6_reg7.csv") %>%
  tibble() %>%
  subset(select = -1)

## 3. Clean Data
table3_results <- full_join(table3_reg1, table3_reg2, by = "coef") %>%
  full_join(table3_reg3, by = "coef") %>%
  full_join(table3_reg4, by = "coef") %>%
  full_join(table3_reg5, by = "coef") %>%
  full_join(table3_reg6, by = "coef")

table3_relevant_variables <- c("bx", "pxp", "bx_pxp", "pxt1", "pxt2", "pxt3", "bx_pxt1", "bx_pxt2", "bx_pxt3")
table3 <- table3_results %>% filter(coef %in% table3_relevant_varaibles)
table3 <- table3 %>% subset(
  select =
    c(
      coef,
      estimate_1, se_1, ci_1, significant_1,
      estimate_2, se_2, ci_2, significant_2,
      estimate_3, se_3, ci_3, significant_3,
      estimate_4, se_4, ci_4, significant_4,
      estimate_5, se_5, ci_5, significant_5,
      estimate_6, se_6, ci_6, significant_6
    )
)

table3[, c(2, 6, 10, 14, 18, 22)] <- lapply(table3[, c(2, 6, 10, 14, 18, 22)], function(x) as.character(round(x, 3)))
table3[, c(3, 7, 11, 15, 19, 23)] <- lapply(table3[, c(3, 7, 11, 15, 19, 23)], function(x) as.character(round(x, 2)))

table3 = table3[c(1,2,3,4,6,8,5,7),]

for (i in 1:nrow(table3)) {
  for (j in c(5, 9, 13, 17, 21, 25)) {
    if (!is.na(table3[i, j]) && table3[i, j] == "yes") {
      table3[i, j - 3] <- paste0(table3[i, j - 3], "*")
    }
  }
}

for (i in 1:nrow(table3)) {
  for (j in c(3, 7, 11, 15, 19, 23)) {
    if (!is.na(table3[i, j])) {
      table3[i, j - 1] <- paste0(table3[i, j - 1], " ", "(", table3[i, j], ")")
    }
  }
}

nrows_table <- nrow(table3)
for (i in 1:nrow(table3)) {
  j <- nrow(table3)
  table3 <- table3 %>% add_row(.before = ((j - nrows_table) + (i + 1)))
  print(table3)
}

for (i in seq(1, nrow(table3), by = 2)) {
  for (j in c(4, 8, 12, 16, 20, 24)) {
    table3[i + 1, j - 2] <- table3[i, j]
  }
}

table3 <- table3 %>% subset(select = -c(3, 4, 5, 7, 8, 9, 11, 12, 13, 15, 16, 17, 19, 20, 21, 23, 24, 25))


banana
table3_gt <- gt(table3)
table3_gt <- table3_gt %>% 
  tab_header(
    title = md("**Table 3**"),
    subtitle = "Effects of the Seasonal Intensification of Fumigations on Birth Weight"
  )
table3_gt <- table3_gt %>% 
  tab_source_note(source_note = md("*bx* = I(Banana Exposure)")) %>%
  tab_source_note(source_note = md("*pxp* = I(Intensive Fumigation)")) %>%
  tab_source_note(source_note = md("*pxtN* = I(Intensive Fumigation during Nth trimester)"))

table3_gt = table3_gt %>%
  cols_label(
    coef = "",
    estimate_1 = "Model 1",
    estimate_2 = "Model 2",
    estimate_3 = "Model 3",
    estimate_4 = "Model 4",
    estimate_5 = "Model 5",
    estimate_6 = "Model 6"
  ) %>% 
  cols_align(
    align = 'center',
    columns = everything())
    table3_gt
  

table3_gt <- table3_gt %>%
  text_case_match(
    NA ~ " ",
    .locations = cells_body(columns = everything())
    )

table3_gt <- table3_gt %>%
  cols_width(
    1 ~ px(70),
    c(2, 3, 4, 5, 6, 7) ~ px(150)
  )
table3_gt <- table3_gt %>%
  tab_style(
    style = list(
      cell_text(color = "#999799"),
      cell_fill(color = "#ffffff")
    ),
    locations = cells_body(rows = c(2, 4, 6, 8, 10, 12, 14, 16))
  )
  table3_gt
table3_gt = table3_gt %>%
  tab_style_body(
    style = list(
      cell_text(color = "#000000", weight = "bold"),
      cell_fill(color = "#ffffff")
    ),
    values = c('bx'),
    targets = 'column'
  )

table3_gt <- table3_gt %>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("bottom"),
        color = '#FFFFFF',
        weight = px(2)
      )
    ),
    locations = list(
      cells_body(
        columns = everything(),
        rows = c(1,3,5,7,9,11,13,15)
      )
      )
    )
    table3_gt
