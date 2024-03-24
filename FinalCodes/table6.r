## 1. Load the data
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
library(tidyselect)
library(webshot) # To save the table as an image
webshot::install_phantomjs()

setwd("C:/Users/vinic/OneDrive/Mestrado/5Tri/EnvironmentalUrban/ReplicationPackage")
tom_azul = "#21549d"

table5_reg1 = read.csv("regressions_results/table5_reg1.csv") %>%
  tibble() %>%
  subset(select = -1)
table5_reg2 = read.csv("regressions_results/table5_reg2.csv") %>%
  tibble() %>%
  subset(select = -1)
table5_reg3 = read.csv("regressions_results/table5_reg3.csv") %>%
  tibble() %>%
  subset(select = -1)
table5_reg4 = read.csv("regressions_results/table5_reg4.csv") %>%
  tibble() %>%
  subset(select = -1)
table5_reg5 = read.csv("regressions_results/table5_reg5.csv") %>%
  tibble() %>%
  subset(select = -1)
table5_reg6 = read.csv("regressions_results/table5_reg6.csv") %>%
  tibble() %>%
  subset(select = -1)
table5_reg7 = read.csv("regressions_results/table5_reg7.csv") %>%
  tibble() %>%
  subset(select = -1)
table5_reg8 = read.csv("regressions_results/table5_reg8.csv") %>%
  tibble() %>%
  subset(select = -1)

## 2. Organize it
table5_results = full_join(table5_reg1, table5_reg2, by = "coef") %>%
  full_join(table5_reg3, by = "coef") %>%
  full_join(table5_reg4, by = "coef") %>%
  full_join(table5_reg5, by = "coef") %>%
  full_join(table5_reg6, by = "coef") %>%
  full_join(table5_reg7, by = "coef") %>%
  full_join(table5_reg8, by = "coef")

table5_relevant_variables = c("bx", "pxp", "bx_pxp", "pxt1", "pxt2", "pxt3", "bx_pxt1", "bx_pxt2", "bx_pxt3")

table5_results = table5_results %>% filter(coef %in% table5_relevant_variables)
table5 = table5_results %>% subset(
  select =
    c(
      coef,
      estimate_1, se_1, ci_1, significant_1,
      estimate_2, se_2, ci_2, significant_2,
      estimate_3, se_3, ci_3, significant_3,
      estimate_4, se_4, ci_4, significant_4,
      estimate_5, se_5, ci_5, significant_5,
      estimate_6, se_6, ci_6, significant_6,
      estimate_7, se_7, ci_7, significant_7,
      estimate_8, se_8, ci_8, significant_8
    )
)

table5[, c(2, 6, 10, 14, 18, 22, 26, 30)] = lapply(table5[, c(2, 6, 10, 14, 18, 22, 26, 30)], function(x) as.character(round(x, 3)))
table5[, c(3, 7, 11, 15, 19, 23, 27, 31)] = lapply(table5[, c(3, 7, 11, 15, 19, 23, 27, 31)], function(x) as.character(round(x, 2)))

table5 = table5[c(1,2,3,4,6,8,5,7),]
view(table5)

for (i in 1:nrow(table5)) {
  for (j in c(5, 9, 13, 17, 21, 25, 29, 33)) {
    if (!is.na(table5[i, j]) && table5[i, j] == "yes") {
      table5[i, j - 3] = paste0(table5[i, j - 3], "*")
    }
  }
}

for (i in 1:nrow(table5)) {
  for (j in c(3, 7, 11, 15, 19, 23, 27, 31)) {
    if (!is.na(table5[i, j])) {
      table5[i, j - 1] = paste0(table5[i, j - 1], " ", "(", table5[i, j], ")")
    }
  }
}

nrows_table = nrow(table5)
for (i in 1:nrow(table5)) {
  j = nrow(table5)
  table5 = table5 %>% add_row(.before = ((j - nrows_table) + (i + 1)))
  print(table5)
}

for (i in seq(1, nrow(table5), by = 2)) {
  for (j in c(4, 8, 12, 16, 20, 24, 28, 32)) {
    table5[i + 1, j - 2] = table5[i, j]
  }
}

table5 = table5 %>% subset(select = -c(3, 4, 5, 7, 8, 9, 11, 12, 13, 15, 16, 17, 19, 20, 21, 23, 24, 25, 27, 28, 29, 31, 32, 33))


## 3. Make the table
table5_gt = gt(table5)

# Create titles
table5_gt = table5_gt %>% 
  tab_header(
    title = md("**Table 5**"),
    subtitle = "Effects of the Seasonal Intensification of Fumigations on Birth Weight and Gestation
Weeks: Maternal Fixed Effects"
  ) # Create titles

#Create footnotes
table5_gt = table5_gt %>% 
  tab_source_note(source_note = md("*bx* = I(Banana Exposure)")) %>%
  tab_source_note(source_note = md("*pxp* = I(Intensive Fumigation)")) %>%
  tab_source_note(source_note = md("*pxtN* = I(Intensive Fumigation during Nth trimester)")) %>%
  tab_source_note(source_note = md("*bx_pxp* = I(Banana Exposure) x I(Intensive Fumigation)")) %>%
  tab_source_note(source_note = md("*bx_pxtN* = I(Banana Exposure) x I(Intensive Fumigation during Nth trimester)")) %>%
  tab_source_note(source_note = md("Robust Standard Errors in Parentheses;  *p < 0.05"))

# Make the tab spanners
table5_gt = table5_gt %>% 
  tab_spanner(
    label = md("**Birth Weight**"),
    columns = c(2,3,4,5)
  ) %>% 
  tab_spanner(
    label = md("**Gestation Weeks**"),
    columns = c(6,7,8,9)
  )
  table5_gt

#Rename columns 
table5_gt = table5_gt %>%
  cols_label(
    coef = "",
    estimate_1 = md("**Model 1**"),
    estimate_2 = md("**Model 2**"),
    estimate_3 = md("**Model 3**"),
    estimate_4 = md("**Model 4**"),
    estimate_5 = md("**Model 5**"),
    estimate_6 = md("**Model 6**"),
    estimate_7 = md("**Model 7**"),
    estimate_8 = md("**Model 8**"))

table5_gt


# Align columns properly
table5_gt = table5_gt %>%
  cols_align(
    align = 'center',
    columns = everything()) %>%
  cols_align(
    align = 'left',
    columns = 1
  )


# Remove all NAs from the table
table5_gt = table5_gt %>%
  text_case_match(
    NA ~ " ",
    .locations = cells_body(columns = everything())
    )
table5_gt

# Set columns width
table5_gt = table5_gt %>%
  cols_width(
    1 ~ px(70),
    c(2, 3, 4, 5, 6, 7, 8, 9) ~ px(150)
  )
table5_gt

# Change color of the CI to light gray
table5_gt = table5_gt %>%
  tab_style(
    style = list(
      cell_text(color = "#7f7f7f"),
      cell_fill(color = "#ffffff")
    ),
    locations = cells_body(rows = c(2, 4, 6, 8, 10, 12, 14, 16))
  )
table5_gt

# Make coefficients bold
table5_gt = table5_gt %>%
  tab_style_body(
    style = list(
      cell_text(color = "#000000", weight = "bold"),
      cell_fill(color = "#ffffff")
    ),
    values = c('bx'),
    targets = 'column'
  )
table5_gt

# Remove horizontal lines of CI rows
table5_gt = table5_gt %>%
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
table5_gt

# Change colors of the borders (tom_azul is set in the beginning of the script)
table5_gt = table5_gt %>%
  tab_options(
    table.border.top.width = px(4),
    table.border.top.color = tom_azul,
    table.border.bottom.width = px(4),
    table.border.bottom.color = tom_azul,
    heading.border.bottom.width = px(2),
    heading.border.bottom.color =tom_azul,
    column_labels.border.top.width = NULL,
    column_labels.border.top.color = NULL,
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = tom_azul,
    table_body.border.bottom.width = NULL,
    table_body.border.bottom.color = tom_azul,
  )


# Save your gt table as an HTML file
gtsave(table6_gt, "final_tables/table5_gt.html")