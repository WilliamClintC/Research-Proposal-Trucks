# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stargazer)
library(broom)

# Load the data from the CSV file
df <- read.csv("C:/Users/clint/Desktop/Research-Paper-Trucks/Exploration/5/df_6.csv")

# Convert REPORT_DATE to Date type
df <- df %>%
  mutate(REPORT_DATE = as.Date(REPORT_DATE))

# Filter data for years 2006 and earlier
df <- df %>%
  filter(format(REPORT_DATE, "%Y") <= 2006)

# Create a dummy variable for FATALITIES
df <- df %>%
  mutate(FATALITIES_DUMMY = ifelse(FATALITIES >= 3, 1, 0))

# Create time indicator variables for each 2-year period
df <- df %>%
  mutate(year_group = floor(as.numeric(format(REPORT_DATE, "%Y")) / 2) * 2)

year_groups <- unique(df$year_group)
for (year_group in year_groups) {
  df <- df %>%
    mutate(!!paste0("indicator_", year_group) := ifelse(year_group == !!year_group, 1, 0))
}

# Create interaction terms between each time indicator and FATALITIES_DUMMY
for (year_group in year_groups) {
  df <- df %>%
    mutate(!!paste0("interaction_", year_group) := !!sym(paste0("indicator_", year_group)) * FATALITIES_DUMMY)
}

# Remove interaction_1994 from the interaction terms
interaction_terms <- paste0("interaction_", year_groups[year_groups != 1994], collapse = " + ")

# Run the original event study model with all interaction terms except interaction_1994
formula <- as.formula(paste("change_2006_2016 ~", interaction_terms))
model1 <- lm(formula, data = df)

# Ensure simple_family is treated as a factor
df <- df %>%
  mutate(simple_family = as.factor(simple_family))

# Run the new event study model with all interaction terms except interaction_1994 and simple_family variable
formula_with_simple_family <- as.formula(paste("change_2006_2016 ~", interaction_terms, "+ simple_family"))
model2 <- lm(formula_with_simple_family, data = df)

# Ensure Region is treated as a factor
df <- df %>%
  mutate(Region = as.factor(Region))

# Run the new event study model with all interaction terms except interaction_1994, simple_family, and Region variable
formula_with_simple_family_and_region <- as.formula(paste("change_2006_2016 ~", interaction_terms, "+ simple_family + Region"))
model3 <- lm(formula_with_simple_family_and_region, data = df)

# Ensure STATE is treated as a factor
df <- df %>%
  mutate(STATE = as.factor(STATE))

# Run the new event study model with all interaction terms except interaction_1994, simple_family, and STATE variable
formula_with_simple_family_and_state <- as.formula(paste("change_2006_2016 ~", interaction_terms, "+ simple_family + STATE"))
model4 <- lm(formula_with_simple_family_and_state, data = df)

# Summary of the models
summary(model1)
summary(model2)
summary(model3)
summary(model4)

# Display the model summaries using stargazer
stargazer(model1, model2, model3, model4, type = "html", title = "Event Study Model Results", 
          column.labels = c("Original Model", "Model with Simple Family", "Model with Simple Family and Region", "Model with Simple Family and State"), 
          out = "C:/Users/clint/Desktop/Research-Paper-Trucks/Exploration/5/model_summary_combined.html")

# Extract coefficients and confidence intervals for model1
coefficients_model1 <- tidy(model1)
coefficients_model1 <- coefficients_model1 %>%
  filter(term != "(Intercept)") %>%
  mutate(year_group = as.numeric(gsub("interaction_", "", term)))

# Extract coefficients and confidence intervals for model2
coefficients_model2 <- tidy(model2)

# Separate interaction terms and simple_family coefficients for model2
interaction_coefficients_model2 <- coefficients_model2 %>%
  filter(grepl("interaction_", term)) %>%
  mutate(year_group = as.numeric(gsub("interaction_", "", term)))

simple_family_coefficients_model2 <- coefficients_model2 %>%
  filter(grepl("simple_family", term)) %>%
  mutate(simple_family = gsub("simple_family", "", term))

# Extract coefficients and confidence intervals for model3
coefficients_model3 <- tidy(model3)

# Separate interaction terms, simple_family coefficients, and Region coefficients for model3
interaction_coefficients_model3 <- coefficients_model3 %>%
  filter(grepl("interaction_", term)) %>%
  mutate(year_group = as.numeric(gsub("interaction_", "", term)))

simple_family_coefficients_model3 <- coefficients_model3 %>%
  filter(grepl("simple_family", term)) %>%
  mutate(simple_family = gsub("simple_family", "", term))

region_coefficients_model3 <- coefficients_model3 %>%
  filter(grepl("Region", term)) %>%
  mutate(Region = gsub("Region", "", term))

# Extract coefficients and confidence intervals for model4
coefficients_model4 <- tidy(model4)

# Separate interaction terms, simple_family coefficients, and STATE coefficients for model4
interaction_coefficients_model4 <- coefficients_model4 %>%
  filter(grepl("interaction_", term)) %>%
  mutate(year_group = as.numeric(gsub("interaction_", "", term)))

simple_family_coefficients_model4 <- coefficients_model4 %>%
  filter(grepl("simple_family", term)) %>%
  mutate(simple_family = gsub("simple_family", "", term))

state_coefficients_model4 <- coefficients_model4 %>%
  filter(grepl("STATE", term)) %>%
  mutate(STATE = gsub("STATE", "", term))

# Plot the coefficients for model1
ggplot(coefficients_model1, aes(x = year_group, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Coefficients of Interaction Terms VS Change in Truck Stops (Original Model)",
       x = "Year Group",
       y = "Coefficient Estimate") +
  theme_minimal()

# Plot the interaction coefficients for model2
ggplot(interaction_coefficients_model2, aes(x = year_group, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Coefficients of Interaction Terms VS Change in Truck Stops (Model with Simple Family)",
       x = "Year Group",
       y = "Coefficient Estimate") +
  theme_minimal()

# Plot the simple_family coefficients for model2
ggplot(simple_family_coefficients_model2, aes(x = simple_family, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Coefficients of Simple Family VS Change in Truck Stops (Model with Simple Family)",
       x = "Simple Family",
       y = "Coefficient Estimate") +
  theme_minimal()

# Plot the interaction coefficients for model3
ggplot(interaction_coefficients_model3, aes(x = year_group, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Coefficients of Interaction Terms VS Change in Truck Stops (Model with Simple Family and Region)",
       x = "Year Group",
       y = "Coefficient Estimate") +
  theme_minimal()

# Plot the simple_family coefficients for model3
ggplot(simple_family_coefficients_model3, aes(x = simple_family, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Coefficients of Simple Family VS Change in Truck Stops (Model with Simple Family and Region)",
       x = "Simple Family",
       y = "Coefficient Estimate") +
  theme_minimal()

# Plot the Region coefficients for model3
ggplot(region_coefficients_model3, aes(x = Region, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Coefficients of Region VS Change in Truck Stops (Model with Simple Family and Region)",
       x = "Region",
       y = "Coefficient Estimate") +
  theme_minimal()

# Plot the interaction coefficients for model4
ggplot(interaction_coefficients_model4, aes(x = year_group, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Coefficients of Interaction Terms VS Change in Truck Stops (Model with Simple Family and State)",
       x = "Year Group",
       y = "Coefficient Estimate") +
  theme_minimal()

# Plot the simple_family coefficients for model4
ggplot(simple_family_coefficients_model4, aes(x = simple_family, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Coefficients of Simple Family VS Change in Truck Stops (Model with Simple Family and State)",
       x = "Simple Family",
       y = "Coefficient Estimate") +
  theme_minimal()

# Plot the STATE coefficients for model4
ggplot(state_coefficients_model4, aes(x = STATE, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(title = "Coefficients of STATE VS Change in Truck Stops (Model with Simple Family and State)",
       x = "STATE",
       y = "Coefficient Estimate") +
  theme_minimal()