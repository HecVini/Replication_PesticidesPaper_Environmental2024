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
#webshot::install_phantomjs()
library(here)
tom_azul = "#21549d"

table3_reg1 = read.csv("ReplicatedResults/table3_reg1.csv") %>%
  tibble() %>%
  subset(select = -1)
table3_reg2 = read.csv("ReplicatedResults/table3_reg2.csv") %>%
  tibble() %>%
  subset(select = -1)
table3_reg3 = read.csv("ReplicatedResults/table3_reg3.csv") %>%
  tibble() %>%
  subset(select = -1)
table3_reg4 = read.csv("ReplicatedResults/table3_reg4.csv") %>%
  tibble() %>%
  subset(select = -1)
table3_reg5 = read.csv("ReplicatedResults/table3_reg5.csv") %>%
  tibble() %>%
  subset(select = -1)
table3_reg6 = read.csv("ReplicatedResults/table3_reg6.csv") %>%
  tibble() %>%
  subset(select = -1)

## 2. Organize it
table3_results = full_join(table3_reg1, table3_reg2, by = "coef") %>%
  full_join(table3_reg3, by = "coef") %>%
  full_join(table3_reg4, by = "coef") %>%
  full_join(table3_reg5, by = "coef") %>%
  full_join(table3_reg6, by = "coef")

table3_relevant_variables = c("bx", "pxp", "bx_pxp", "pxt1", "pxt2", "pxt3", "bx_pxt1", "bx_pxt2", "bx_pxt3")

table3_results = table3_results %>% filter(coef %in% table3_relevant_variables)
table3 = table3_results %>% subset(
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

table3[, c(2, 6, 10, 14, 18, 22)] = lapply(table3[, c(2, 6, 10, 14, 18, 22)], function(x) as.character(round(x, 3)))
table3[, c(3, 7, 11, 15, 19, 23)] = lapply(table3[, c(3, 7, 11, 15, 19, 23)], function(x) as.character(round(x, 2)))

table3 = table3[c(1,2,3,4,6,8,5,7),]

for (i in 1:nrow(table3)) {
  for (j in c(5, 9, 13, 17, 21, 25)) {
    if (!is.na(table3[i, j]) && table3[i, j] == "yes") {
      table3[i, j - 3] = paste0(table3[i, j - 3], "*")
    }
  }
}

for (i in 1:nrow(table3)) {
  for (j in c(3, 7, 11, 15, 19, 23)) {
    if (!is.na(table3[i, j])) {
      table3[i, j - 1] = paste0(table3[i, j - 1], " ", "(", table3[i, j], ")")
    }
  }
}

nrows_table = nrow(table3)
for (i in 1:nrow(table3)) {
  j = nrow(table3)
  table3 = table3 %>% add_row(.before = ((j - nrows_table) + (i + 1)))
  print(table3)
}

for (i in seq(1, nrow(table3), by = 2)) {
  for (j in c(4, 8, 12, 16, 20, 24)) {
    table3[i + 1, j - 2] = table3[i, j]
  }
}

table3 = table3 %>% subset(select = -c(3, 4, 5, 7, 8, 9, 11, 12, 13, 15, 16, 17, 19, 20, 21, 23, 24, 25))

## 3. Make the table
table3_gt = gt(table3)

# Create titles
table3_gt = table3_gt %>% 
  tab_header(
    title = md("**Table 3**"),
    subtitle = "Effects of the Seasonal Intensification of Fumigations on Birth Weight"
  )

#Create footnotes
table3_gt = table3_gt %>% 
  tab_source_note(source_note = md("*bx* = I(Banana Exposure)")) %>%
  tab_source_note(source_note = md("*pxp* = I(Intensive Fumigation)")) %>%
  tab_source_note(source_note = md("*pxtN* = I(Intensive Fumigation during Nth trimester)")) %>%
  tab_source_note(source_note = md("*bx_pxtN* = I(Banana Exposure) x I(Intensive Fumigation during Nth trimester)")) %>%
  tab_source_note(source_note = md("Robust Standard Errors in Parentheses;  *p < 0.05"))


#Rename columns 
table3_gt = table3_gt %>%
  cols_label(
    coef = "",
    estimate_1 = md("**Model 1**"),
    estimate_2 = md("**Model 2**"),
    estimate_3 = md("**Model 3**"),
    estimate_4 = md("**Model 4**"),
    estimate_5 = md("**Model 5**"),
    estimate_6 = md("**Model 6**"))


# Make significant coefficients bold (couldnt find an authomatic way to do it)
table3_gt = table3_gt %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(rows = 3, columns = 3)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(rows = 5, columns = 3)
  ) %>% 
    tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_body(rows = 13, columns = 4)
    ) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(rows = 15, columns = 5)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(rows = c(13,15), columns = 7)
  )

# Align columns properly
table3_gt = table3_gt %>%
  cols_align(
    align = 'center',
    columns = everything()) %>%
  cols_align(
    align = 'left',
    columns = 1
  )


# Remove all NAs from the table
table3_gt = table3_gt %>%
  text_case_match(
    NA ~ " ",
    .locations = cells_body(columns = everything())
    )
table3_gt

# Set columns width
table3_gt = table3_gt %>%
  cols_width(
    1 ~ px(70),
    c(2, 3, 4, 5, 6, 7) ~ px(150)
  )
table3_gt

# Change color of the CI to light gray
table3_gt = table3_gt %>%
  tab_style(
    style = list(
      cell_text(color = "#7f7f7f"),
      cell_fill(color = "#ffffff")
    ),
    locations = cells_body(rows = c(2, 4, 6, 8, 10, 12, 14, 16))
  )


# Make coefficients bold
table3_gt = table3_gt %>%
  tab_style_body(
    style = list(
      cell_text(color = "#000000", weight = "bold"),
      cell_fill(color = "#ffffff")
    ),
    values = c('bx'),
    targets = 'column'
  )


# Remove horizontal lines of CI rows
table3_gt = table3_gt %>%
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


# Change colors of the borders (tom_azul is set in the beginning of the script)
table3_gt = table3_gt %>%
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
table3_gt
gtsave(table3_gt, "FinalTables/table3.html")
