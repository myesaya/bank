library(tidyverse)
library(janitor)
library(tidyquant)
library(patchwork)
library(survival)
library(survminer)
library(gtsummary)
library(gt)
library(flextable)
library(psych)
library(stringr)
library(haven)
library(labelled)
library(forcats)
library(broom)
library(treemapify)
library(ggalluvial)
library(ggthemes)
library(scales)
library(MASS)  # For ordinal regression (polr)
library(dplyr)
library(naniar)
library(sjPlot)
library(ordinal)
library(lsr)
library(car)
library(splitstackshape)


# 1. Read the dataset with full survey questions
labels <- read_csv(here::here("data/raw/labels.csv")) |>
  mutate(across(character(), as.factor))

# 2. Clean column names by removing prefixes AND numbering patterns
clean_header <- function(x) {
  x |>
    # Remove x-prefixes (e.g., "x1_")
    str_remove("^x\\d+_") |>
    # Remove numbering patterns (e.g., "1. ", "8a. ")
    str_remove("^\\d+[a-z]?\\.\\s*")
}

# 3. Save original column names BEFORE cleaning
original_labels <- colnames(labels)

# 4. Apply cleaning to both:
#   - Variable labels (for final dataset)
#   - Column names (for intermediate processing)
variable_labels <- original_labels |> map_chr(clean_header)
colnames(labels) <- colnames(labels) |> map_chr(clean_header)

# 5. Read clean variable names
variables <- read_csv(here::here("data/raw/variables.csv")) |>
  clean_names() |> 
  mutate(across(character(), as.factor))

# 6. Create working dataframe
df <- labels

# 7. Apply clean variable names to df
colnames(df) <- colnames(variables)

# 8. Assign cleaned question text as variable labels
for (i in seq_along(df)) {
  var_label(df[[i]]) <- variable_labels[i]
}



formatted_output <- df |>
  # Convert all data to character
  mutate(across(everything(), as.character)) |>
  # Add variable names row (second header)
  {function(x) add_row(x, !!!setNames(as.list(colnames(x)), colnames(x)), .before = 1)}() |>
  # Add original labels row (first header)
  {function(y) add_row(y, !!!setNames(as.list(original_labels), colnames(y)), .before = 1)}()


# Clean environment (keep both df and formatted_output)
rm(list = setdiff(ls(), c("df", "formatted_output")))

# 6. Write to CSV with no headers
write_excel_csv(
  formatted_output,
  here::here("data/processed/survey-structured.csv"),
  col_names = FALSE
)
#cleaning
df <- df %>%
  dplyr::select(-c(start_time:last_modified_time))

levels(df$education_level)

# Demographic variables
library(dplyr)
library(forcats)

df <- df |>
  mutate(education_level = factor(education_level,
                                  levels = c("Secondary School",
                                             "Diploma",
                                             "Bachelor's Degree",
                                             "Postgraduate Degree",
                                             "Other (please specify) __________")),
         education_level = fct_recode(education_level,
                                      Other = "Other (please specify) __________"))


   


summary_table <- df %>%
  dplyr::select(
    gender, age, education_level,
    social_media_frequency
  ) %>%
  gtsummary::tbl_summary(
    type = list(all_categorical() ~ "categorical"),
    label = list(
      gender ~ "Gender",
      age ~ "Age Group",
      education_level ~ "Education Level",
      social_media_frequency ~ "Social media use frequency"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  )

summary_table

# Step 2: Extract and transform the summary table
summary_df <- summary_table$table_body %>%
  dplyr::rename(Characteristic = variable, Category = label) %>%
  dplyr::mutate(
    Frequency = stringr::str_extract(stat_0, "^[^\\(]+") %>% stringr::str_trim(),
    Percentage = stringr::str_extract(stat_0, "(?<=\\()[0-9.]+(?=%\\))")
  ) %>%
  dplyr::mutate(
    Frequency = na_if(Frequency, ""),
    Percentage = na_if(Percentage, "")
  ) %>%
  dplyr::group_by(Characteristic) %>%
  dplyr::mutate(
    Characteristic = if_else(row_number() == 1, Characteristic, ""),
    Category = if_else(row_number() == 1, paste(Characteristic, Category, sep = " | "), Category)
  ) %>%
  dplyr::mutate(
    Characteristic = if_else(Characteristic != "", Category, Characteristic),
    Category = if_else(Characteristic == Category, "", Category)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Characteristic = stringr::str_replace(Characteristic, ".*\\| ", "")) %>%
  dplyr::select(Characteristic, Category, Frequency, Percentage)

# Step 3: Create a formatted gt table
gt_table <- summary_df %>%
  gt() %>%
  tab_header(
    title = "Survey Summary Table"
  ) %>%
  cols_label(
    Characteristic = "Variable",
    Category = "Choices",
    Frequency = "Frequency",
    Percentage = "Percentage"
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = ""
  )

# Step 4: Display and save the table
print(gt_table)
gtsave(gt_table, filename = "survey_summary_table.docx")

# Objective 1: Analyze Customer Preferences for Different So --------
#Goal: Identify which platforms/content types customers prefer for banking interactions.
library(dplyr)
library(tidyr)
library(stringr)

df <- df %>%
  mutate(socio_media_name = str_remove_all(socio_media_name, ";+$"),
         row = row_number()) %>%
  separate_rows(socio_media_name, sep = ";") %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = socio_media_name, values_from = value, values_fill = 0) %>%
  dplyr::select(-row)


#plot version 1
plot <- df |>
  dplyr::select(id,Instagram:Twitter) |>
  drop_na() |>
  mutate(across(Instagram:Twitter, ~ ifelse(. == 1, "Yes", "No")))


usefulness <- plot |>
  pivot_longer(
    cols = Instagram:Twitter,
    names_to = "how",
    values_to = "Response"
  ) |>  
  
  mutate(how = as.factor(how)) |>
  mutate(how = fct_recode(how,
                          other = "Other- please specify"))
                          
                          
#######plotting
mhm<-usefulness |>
  filter(Response == "Yes") |>
  drop_na() |>
  group_by(how) |>
  summarise(
    Count = n(),
    Percentage = (Count / n_distinct(usefulness$id)) * 100
  )  |>
  ggplot(aes(x = fct_reorder(how, Percentage, .desc = TRUE), y = Percentage, fill = how)) +
  geom_col(width = 0.45) +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +  # Ensure Y-axis goes up to 100%
  labs(title="Social Media Popularity",
       x = "Name",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),  # Black title
    axis.title.x = element_text(hjust = 0.5, size = 14, color = "black"),  # Black X-axis title
    axis.title.y = element_text(hjust = 0.5, size = 14, color = "black"),  # Black Y-axis title
    axis.text.x = element_text(size = 13, color = "black", angle = 45, hjust = 1),  # Black X-axis text
    axis.text.y = element_text(size = 13, color = "black")   # Black Y-axis text
  ) +
  guides(fill = "none")
mhm

ggsave("prefered social media.pdf",
       plot = mhm,
       width = 9, height = 6,
       units = "in", device = "pdf")
#Chi-Square Test for demographic targetting (age)

df |> 
  dplyr::select(age,Instagram:Twitter) |>
  tbl_summary(
    by = age,
    statistic = all_categorical() ~ "{n} ({p}%)",
    label = list(
      "Other- please specify" ~ "Other"
    )
  ) |> 
  add_p() |> 
  bold_p() |> 
  separate_p_footnotes() |> 
  as_flex_table() |>
  save_as_docx(path = "~/gitrepos/bank/age-media usage.docx")


# Objective Evaluate the Influence of Social Media Campaigns on Brand Perception --------
#Goal: Goal: Measure how campaigns affect trust, loyalty, and brand image.





