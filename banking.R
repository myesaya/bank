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


# Objective 2:  Evaluate the Influence of Social Media Campaigns on Brand Perception --------
#Goal: Goal: Measure how campaigns affect trust, loyalty, and brand image.
# Load required libraries
library(tidyverse)
library(ggthemes)
library(likert)
library(sjPlot)



#digital banking platforms used

df <- df %>%
  dplyr::filter(yes_name_digital_platform!='NA') |> 
  mutate(yes_name_digital_platform = str_remove_all(yes_name_digital_platform, ";+$"),
         row = row_number()) %>%
  separate_rows(yes_name_digital_platform, sep = ";") %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = yes_name_digital_platform, values_from = value, values_fill = 0) %>%
  dplyr::select(-row)

glimpse(df)
#plot version 2
plot <- df |>
  dplyr::select(id,"Mobile banking app":"Other (please specify) __________") |>
  drop_na() |>
  mutate(across("Mobile banking app":"Other (please specify) __________", ~ ifelse(. == 1, "Yes", "No")))


usefulness <- plot |>
  pivot_longer(
    cols = "Mobile banking app":"Other (please specify) __________",
    names_to = "how",
    values_to = "Response"
  ) |> 
  
  mutate(how = as.factor(how)) |>
  mutate(how = fct_recode(how,
                          ATM = "ATM services",
                          "Banking app"="Mobile banking app",
                          "USSD banking"="USSD banking (*626#)",
                          "Internet banking"= "Internet banking",
                          Other= "Other (please specify) __________"))

  usefulness$how<-as.factor(usefulness$how)
  levels(usefulness$how)

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
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 100, 20)) +  # Ensure Y-axis goes up to 100%
  labs(title=" Customer-Bank Interaction (Banking Platforms)",
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

ggsave("Customer-Bank Interaction (Banking Platforms).pdf",
       plot = mhm,
       width = 9, height = 6,
       units = "in", device = "pdf")


#purely mobile banking
#######plotting 2
mhm<-usefulness |>
  filter(how!="ATM") |> 
  filter(Response == "Yes") |>
  drop_na() |>
  group_by(how) |>
  summarise(
    Count = n(),
    Percentage = (Count / n_distinct(usefulness$id)) * 100
  )  |>
  ggplot(aes(x = fct_reorder(how, Percentage, .desc = TRUE), y = Percentage, fill = how)) +
  geom_col(width = 0.35) +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 100, 20)) +  # Ensure Y-axis goes up to 100%
  labs(title="Mobile Banking platforms used",
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

ggsave("Mobile Banking platforms used.pdf",
       plot = mhm,
       width = 9, height = 6,
       units = "in", device = "pdf")

#often engagement

# Data Preparation
df<-df |> 
  dplyr::rename(
    engagement = often,
    image=testimonials,
    trust = easy_understand,
    loyalty = recommended,
    influence = socia_has_influenced_digital_use,
    effectiveness = rate_effectiveness
  ) 
# --------------------------
# DESCRIPTIVE ANALYSIS (Visuals)
# --------------------------

# 1. Trust Distribution

data_with_percentages <- df %>%
  drop_na(trust) %>%  # Remove rows where 'trust' is NA
  count(trust) %>%    # Count occurrences of each 'trust' level
  mutate(percentage = n / sum(n) * 100)  # Calculate percentage


data_with_percentages$trust<-as.factor(data_with_percentages)
levels(data_with_percentages$trust)

# Step 2: Create the ggplot
mhm<-ggplot(data_with_percentages, aes(x = fct_reorder(trust, -percentage), 
                                  y = percentage, fill = trust)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 100, 10)) +  # Ensure Y-axis goes up to 100%
  labs(title = "Customers trust in NBM's digital banking",
       subtitle = "Based on Social Media Marketing",
       x="Trust level",
       y = "Percentage (%)") +
  theme_economist() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "black"),  # Black title
    axis.title.x = element_text(hjust = 0.5, size = 14, color = "black",face = "bold"),  # Black X-axis title
    axis.title.y = element_text(hjust = 0.5, size = 14, color = "black"),  # Black Y-axis title
    axis.text.x = element_text(size = 13, color = "black"),  # Black X-axis text
    axis.text.y = element_text(size = 13, color = "black")   # Black Y-axis text
  ) +
  guides(fill = "none")  # Remove legend since colors are for distinction
mhm

ggsave("Customers trust in NBM's digital banking.pdf",
       plot = mhm,
       width = 9, height = 6,
       units = "in", device = "pdf")


#image
# 1. image Distribution

data_with_percentages <- df %>%
  drop_na(image) %>%  # Remove rows where 'trust' is NA
  count(image) %>%    # Count occurrences of each 'trust' level
  mutate(percentage = n / sum(n) * 100)  # Calculate percentage

# Step 2: Create the ggplot
mhm<-ggplot(data_with_percentages, aes(x = fct_reorder(image, -percentage), 
                                  y = percentage, fill = image)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 100, 10)) +  # Ensure Y-axis goes up to 100%
  labs(title = "Brand image of NBM's digital banking",
       subtitle = "Based on Social Media Testimonials",
       x="Frequency",
       y = "Percentage (%)") +
  theme_economist() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "black"),  # Black title
    axis.title.x = element_text(hjust = 0.5, size = 14, color = "black",face = "bold"),  # Black X-axis title
    axis.title.y = element_text(hjust = 0.5, size = 14, color = "black"),  # Black Y-axis title
    axis.text.x = element_text(size = 13, color = "black"),  # Black X-axis text
    axis.text.y = element_text(size = 13, color = "black")   # Black Y-axis text
  ) +
  guides(fill = "none")  # Remove legend since colors are for distinction

mhm
ggsave("Brand image of NBM's digital banking.pdf",
       plot = mhm,
       width = 9, height = 6,
       units = "in", device = "pdf")



# 2. Engagement vs. Effectiveness
plot<- df |>
  
  # Calculate counts and percentages within each pigparasite group
  group_by(engagement, effectiveness) |>
  drop_na() |>
  summarise(count = n()) |>
  mutate(percentage = count / sum(count) * 100) |>
  ggplot(aes(x = engagement,
             y = count, fill = effectiveness)) +
  geom_bar(stat = "identity", position = "fill",width=0.6) +  # Position fill scales the bars to 100%
  geom_text(aes(label = paste0(format(round(percentage, 1), nsmall = 1), "%")),
            position = position_fill(vjust = 0.5), size = 5, fontface = "bold") +  # Add percentage labels on the bars
  labs(title="Engagement Frequency vs. Marketing Effectiveness",
       x= "Engagement level",
       y = "Proportion",  # Percentage representation
       fill = "Effectiveness") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),  # Bold, centered black title
    axis.title.x = element_text(hjust = 0.5, size = 14, color = "black", face = "bold"),
    axis.title.y = element_text(hjust = 0.5, size = 14, color = "black"),
    axis.text.x = element_text(size = 13, color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 13, color = "black"),
    legend.position = "right",
    legend.justification = "center",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold")  # Bold legend title
  )

plot

ggsave("Engagement Frequency vs. Marketing Effectiveness.pdf",
       plot = mhm,
       width = 9, height = 6,
       units = "in", device = "pdf")


# 3. Influence on Adoption Decision
data_with_percentages <- df %>%
  drop_na(influence) %>%  # Remove rows where 'trust' is NA
  count(influence) %>%    # Count occurrences of each 'trust' level
  mutate(percentage = n / sum(n) * 100)  # Calculate percentage

# Step 2: Create the ggplot
mhm<-ggplot(data_with_percentages, aes(x = fct_reorder(influence, -percentage), 
                                       y = percentage, fill = influence)) +
  geom_col(width = 0.45) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            vjust = -0.5, size = 5.5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 100, 10)) +  # Ensure Y-axis goes up to 100%
  labs(title = "Social Media Influence on Digital Banking Adoption",
       x="Influence level",
       y = "Percentage (%)") +
  theme_clean() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "black"),  # Black title
    axis.title.x = element_text(hjust = 0.5, size = 14, color = "black",face = "bold"),  # Black X-axis title
    axis.title.y = element_text(hjust = 0.5, size = 14, color = "black"),  # Black Y-axis title
    axis.text.x = element_text(size = 13, color = "black"),  # Black X-axis text
    axis.text.y = element_text(size = 13, color = "black")   # Black Y-axis text
  ) +
  guides(fill = "none")  # Remove legend since colors are for distinction
mhm

ggsave("Social Media Influence on Digital Banking Adoption.pdf",
       plot = mhm,
       width = 9, height = 6,
       units = "in", device = "pdf")

# --------------------------
# INFERENTIAL ANALYSIS
# --------------------------

# 1. Ordinal Regression (Trust ~ Engagement + Effectiveness)
# Load libraries
library(dplyr)
library(gt)
library(MASS)
library(tibble)  # for rownames_to_column()

# Prepare data: ensure 'trust' is an ordered factor
df <- df |>
  mutate(trust = factor(trust,
                        levels = c("Strongly Disagree", 
                                   "Disagree", 
                                   "Neutral", 
                                   "Agree", 
                                   "Strongly Agree"),
                        ordered = TRUE))

# Fit the ordered logistic regression model
trust_model <- polr(trust ~ engagement + effectiveness + image, 
                    data = df, Hess = TRUE)

# Extract coefficient summary
coefs <- as.data.frame(coef(summary(trust_model)))

# Ensure 't value' is numeric
coefs$t.value <- as.numeric(coefs[["t value"]])

# Calculate p-values
coefs$p_value <- 2 * pnorm(abs(coefs$t.value), lower.tail = FALSE)

# Create gt summary table
gt_table <- coefs |>
  rownames_to_column("Predictor") |>
  mutate(across(where(is.numeric), round, 3)) |>
  gt() |>
  tab_header(
    title = "Ordered Logistic Regression Results",
    subtitle = "Predicting Trust (Positive brand perception)"
  ) |>
  cols_label(
    Predictor = "Variable",
    Value = "Estimate",
    t.value = "t value",
    p_value = "p-value"
  )

# Print the gt table
gt_table

gtsave(gt_table, filename = "ordinal.docx")


# Challenges --------------------------------------------------------------


df <- df %>%
  dplyr::filter(challlenge!='NA') |> 
  mutate(challlenge = str_remove_all(challlenge, ";+$"),
         row = row_number()) %>%
  separate_rows(challlenge, sep = ";") %>%
  mutate(value = 1) |> 
  pivot_wider(names_from = challlenge, values_from = value, values_fill = 0) %>%
  dplyr::select(-row)

glimpse(df)
#plot version 1
plot <- df |>
  dplyr::select(id,"Poor internet connectivity":"Other (please specify)") |>
  drop_na() |>
  mutate(across("Poor internet connectivity":"Other (please specify)", ~ ifelse(. == 1, "Yes", "No")))


usefulness <- plot |>
  pivot_longer(
    cols = "Poor internet connectivity":"Other (please specify)",
    names_to = "how",
    values_to = "Response"
  ) |> 
  mutate(how = as.factor(how)) |>
  mutate(how = fct_recode(how,
                          "Difficult registration process " =  "Difficult registration process ",
                          "Lack of clear instructions "="Lack of clear instructions " ,
                          "Other"= "Other (please specify)",
                          "Poor internet connectivity"= "Poor internet connectivity",
                          "Security concerns"= "Security concerns "))


levels(usefulness$how)

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
  geom_col(width = 0.35) +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 75), breaks = seq(0, 100, 20)) +  # Ensure Y-axis goes up to 100%
  labs(title="Challenges in using NBM digital services",
       x = "Challenge",
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

ggsave("Challenges in using NBM digital services.pdf",
       plot = mhm,
       width = 9, height = 6,
       units = "in", device = "pdf")


#####################################################
# Load required libraries
library(tidyverse)
library(ggthemes)
library(scales)
library(ggwordcloud)
library(tm)
library(forcats)
#####easy
# 1. easy Distribution

data_with_percentages <- df %>%
  drop_na(easy) %>%  # Remove rows where 'trust' is NA
  count(easy) %>%    # Count occurrences of each 'trust' level
  mutate(percentage = n / sum(n) * 100)  # Calculate percentage


data_with_percentages$trust<-as.factor(data_with_percentages)
levels(data_with_percentages$trust)

# Step 2: Create the ggplot
mhm<-ggplot(data_with_percentages, aes(x = fct_reorder(easy, -percentage), 
                                       y = percentage, fill = easy)) +
  geom_col(width = 0.45) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 100, 10)) +  # Ensure Y-axis goes up to 100%
  labs(title = "Easiness to navigate NBM banking services",
       x="Easiness level",
       y = "Percentage (%)") +
  theme_clean() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "black"),  # Black title
    axis.title.x = element_text(hjust = 0.5, size = 14, color = "black",face = "bold"),  # Black X-axis title
    axis.title.y = element_text(hjust = 0.5, size = 14, color = "black"),  # Black Y-axis title
    axis.text.x = element_text(size = 13, color = "black"),  # Black X-axis text
    axis.text.y = element_text(size = 13, color = "black")   # Black Y-axis text
  ) +
  guides(fill = "none")  # Remove legend since colors are for distinction
mhm

ggsave("Easiness to navigate NBM's mobile banking.pdf",
       plot = mhm,
       width = 9, height = 6,
       units = "in", device = "pdf")














###suggestions and improvements-word cloud
# Word cloud visualization
# Load required libraries
library(dplyr)
library(tidytext)
install.packages("tidytext")
library(ggplot2)
library(ggwordcloud)
library(stringr)

# Set seed for reproducibility
set.seed(123)

# Filter out NA and preprocess suggestions
word_freq <- df |>
  filter(!is.na(suggestions)) |>
  pull(suggestions) |>
  tolower() |>
  str_replace_all("[^[:alnum:]\\s]", " ") |>
  str_squish() |>
  str_split("\\s+") |>
  unlist() |>
  as_tibble() |>
  rename(word = value) |>
  anti_join(stop_words, by = "word") |>
  count(word, sort = TRUE)

# Create the word cloud
wordcloud_plot <- ggplot(word_freq, aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(shape = "square", rm_outside = TRUE) +
  scale_size_area(max_size = 14) +
  scale_color_gradient(low = "#1f77b4", high = "#d62728") +
  theme_minimal() +
  labs(title = "Key Themes in Customer Improvement Suggestions")

# Show the word cloud
wordcloud_plot

