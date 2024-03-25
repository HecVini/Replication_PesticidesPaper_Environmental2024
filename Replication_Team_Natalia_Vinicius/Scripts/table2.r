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
library(here)


# linter:disable
lint(
    text = strrep("x", 400L),
    linters = line_length_linter(length = 400L)
)

lint()


## 2. Load Data
bananas1.R <- read_dta("OriginalData/bananas1.dta") # Main dataset used on the papaer
bananas1 <- bananas1.R %>% clean_names() # Clean the column names

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
bananas1_variables_description %>% print(n = 100)

## 4. Make Table 2
tom_azul = "#21549d"
columns_table2 <- c(
    "weight", "gweeks", "lbw", "preterm", "age", "dsex1", "deduc1",
    "dethnic6", "tlabor1", "dmarital1", "dmarital2", "dmarital3",
    "dprivate1", "labors", "nchild", "visits", "dnbl1", "esample", "bx", "pxp"
)

colwise_function <- function(data, func, group) {
    func_name <- deparse(substitute(func)) # Get the name of the function
    results <- apply(data, MARGIN = 2, FUN = func, na.rm = TRUE)
    results <- as.list(results) # Transform the means into a list
    results <- tibble(column_name = names(results), mean = unlist(results))
    colnames(results) <- c("column_name", paste0(func_name, "_", group))
    return(results)
} # Generic function to apply a function to each column of a dataframe

colwise_merge <- function(data, group) {
    data1 <- colwise_function(data, mean, group)
    data2 <- colwise_function(data, sd, group)

    merged_data <- full_join(data1, data2, by = "column_name")
    return(merged_data)
} # Generic function to merge two dataframes by the column_name

table2 <- bananas1 %>% subset(select = columns_table2) # Subset data to get the variables used in table 2

table2_1 <- table2 %>% filter(esample == 1) # esample: newborn present in final sample (just 21 observations have newsample != 1)
table2_1 <- table2_1 %>% select(-c(esample, bx, pxp)) # Remove the esample column
table_2_1_observations <- dim(table2_1)[1] # Get the number of observations
table2_1_results <- colwise_merge(table2_1, 1) # Get means and std for this group

table2_2 <- table2 %>% filter(esample == 1, bx == 0) # bx = dummy for exposure to pesticides (1 = yes)
table2_2 <- table2_2 %>% select(-c(esample, bx, pxp)) # Remove the esample and bx columns
table_2_2_observations <- dim(table2_2)[1] # Get the number of observations
table2_2_results <- colwise_merge(table2_2, 2)

table2_3 <- table2 %>% filter(esample == 1, bx == 1)
table2_3 <- table2_3 %>% select(select = -c(esample, bx, pxp)) # Remove the esample column
table_2_3_observations <- dim(table2_3)[1] # Get the number of observations
table2_3_results <- colwise_merge(table2_3, 3) # Get means and std for this group

table2_4 <- table2 %>% filter(esample == 1, pxp == 0) # pxp: exposure to pesticides
table2_4 <- table2_4 %>% select(select = -c(esample, bx, pxp)) # Remove the esample column
table_2_4_observations <- dim(table2_4)[1] # Get the number of observations
table2_4_results <- colwise_merge(table2_4, 4) # Get means and std for this group

table2_5 <- table2 %>% filter(esample == 1, pxp == 1) # pxp: exposure to pesticides
table2_5 <- table2_5 %>% select(select = -c(esample, bx, pxp)) # Remove the esample column
table_2_5_observations <- dim(table2_5)[1] # Get the number of observations
table2_5_results <- colwise_merge(table2_5, 5) # Get means and std for this group
table_2_5_observations
table2_results <- full_join(table2_1_results, table2_2_results, by = "column_name") # Merge the results
table2_results <- full_join(table2_results, table2_3_results, by = "column_name") # Merge the results
table2_results <- full_join(table2_results, table2_4_results, by = "column_name") # Merge the results
table2_results <- full_join(table2_results, table2_5_results, by = "column_name") # Merge the results

table2 <- table2_results
table2[,1] = c('Weight', 
'Gestation Weeks', 
'I(Low Birth Weight)', 
'I(Preterm Birth)', 
'Mother Age', 
'I(Newborn Male)',
'I(Mother Education < High School)',
'I(Newborn Mestizo)',
'I(Normal Birth)',
'I(Mother has Nonmarital Union)',
'I(Mother is Single)',
'I(Mother is Married)',
'I(Born in Public Hospital)',
'No. of Previous Labors',
'No. of Other Children',
'No. of Prenatal Care Visits',
'I(Not Twins)')
nrows_table <- nrow(table2)

table2[, c(2, 4, 6, 8, 10)] <- lapply(table2[, c(2, 4, 6, 8, 10)], function(x) as.character(round(x, 3)))
table2[, c(3, 5, 7, 9, 11)] <- lapply(table2[, c(3, 5, 7, 9, 11)], function(x) as.character(round(x, 2)))
table2

for (i in 1:nrow(table2)) {
    j <- nrow(table2)
    table2 <- table2 %>% add_row(.before = ((j - nrows_table) + (i + 1)))
    print(table2)
}

for (i in seq(1, nrow(table2), by = 2)) {
    for (j in c(3, 5, 7, 9, 11)) {
        table2[i + 1, j - 1] <- paste0("(", table2[i, j], ")")
    }
}
table2 <- table2 %>% subset(select = c(1, 2, 4, 6, 8, 10))


table2 <- table2 %>% add_row(
    column_name = "No. of Observations",
    mean_1 = as.character(table_2_1_observations),
    mean_2 = as.character(table_2_2_observations),
    mean_3 = as.character(table_2_3_observations),
    mean_4 = as.character(table_2_4_observations),
    mean_5 = as.character(table_2_5_observations)
)



table2_gt <- gt(table2)

# Create titles
table2_gt <- table2_gt %>%
    tab_header(
        title = md("**Table 2**"),
        subtitle = "Descriptive Statistics: Means (standard deviations) of Selected Variables"
    ) # Create titles
table2_gt

# Create footnotes
table2_gt <- table2_gt %>%
    tab_source_note(source_note = md("*bx* = I(Banana Exposure)")) %>%
    tab_source_note(source_note = md("*pxp* = I(Intensive Fumigation)"))
table2_gt

# Rename columns
table2_gt <- table2_gt %>%
    cols_label(
        column_name = "",
        mean_1 = md("**Subset 1:** <br> All Newborns"),
        mean_2 = md("**Subset 2:** <br> bx = 0"),
        mean_3 = md("**Subset 3:** <br> bx = 1"),
        mean_4 = md("**Subset 4:** <br> pxp = 0"),
        mean_5 = md("**Subset 5:** <br> pxp = 1")
    )
table2_gt

# Align columns properly
table2_gt <- table2_gt %>%
    cols_align(
        align = "center",
        columns = everything()
    ) %>%
    cols_align(
        align = "left",
        columns = 1
    )
table2_gt

# Remove all NAs from the table
table2_gt <- table2_gt %>%
    text_case_match(
        NA ~ " ",
        .locations = cells_body(columns = everything())
    )
table2_gt

# Set columns width
table2_gt <- table2_gt %>%
    cols_width(
        1 ~ px(200),
        c(2, 3, 4, 5, 6) ~ px(120)
    )
table2_gt

# Make coefficients bold
table2_gt <- table2_gt %>%
    tab_style_body(
        style = list(
            cell_text(color = "#000000", weight = "bold"),
            cell_fill(color = "#ffffff")
        ),
        values = c("weight"),
        targets = "column"
    )
table2_gt

# Remove horizontal lines of CI rows
table2_gt <- table2_gt %>%
    tab_style(
        style = list(
            cell_borders(
                sides = c("bottom"),
                color = "#FFFFFF",
                weight = px(2)
            )
        ),
        locations = list(
            cells_body(
                columns = everything(),
                rows = c(seq(1,35,2))
            )
        )
    )
table2_gt

# Change colors of the borders (tom_azul is set in the beginning of the script)
table2_gt <- table2_gt %>%
    tab_options(
        table.border.top.width = px(4),
        table.border.top.color = tom_azul,
        table.border.bottom.width = px(4),
        table.border.bottom.color = tom_azul,
        heading.border.bottom.width = px(2),
        heading.border.bottom.color = tom_azul,
        column_labels.border.top.width = NULL,
        column_labels.border.top.color = NULL,
        column_labels.border.bottom.width = px(2),
        column_labels.border.bottom.color = tom_azul,
        table_body.border.bottom.width = NULL,
        table_body.border.bottom.color = tom_azul,
        footnotes.border.bottom.color = tom_azul
    )

# Save your gt table as an HTML file
gtsave(table2_gt, "FinalTables/table2.html")
