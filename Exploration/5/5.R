# Load required libraries
library(dplyr)
library(ggplot2)
library(stargazer)
library(broom)
library(fixest)

# Load and prepare data
df <- read.csv("C:/Users/clint/Desktop/Research-Paper-Trucks/Exploration/5/df_6.csv") %>%
  # Convert date and filter for years 2006 and earlier
  mutate(REPORT_DATE = as.Date(REPORT_DATE)) %>%
  filter(format(REPORT_DATE, "%Y") <= 2006) %>%
  # Create fatalities dummy (1 if fatalities >= 3, otherwise 0)
  mutate(FATALITIES_DUMMY = ifelse(FATALITIES >= 3, 1, 0)) %>%
  # Group years into 2-year periods
  mutate(year_group = floor(as.numeric(format(REPORT_DATE, "%Y")) / 2) * 2) %>%
  # Convert categorical variables to factors
  mutate(
    simple_family = as.factor(simple_family),
    Region = as.factor(Region),
    STATE = as.factor(STATE)
  )

# Create time indicators and interaction terms
year_groups <- unique(df$year_group)
earliest_year <- min(year_groups)

# Create time indicators and interaction terms in a single pass
for (year_group in year_groups) {
  indicator_name <- paste0("indicator_", year_group)
  interaction_name <- paste0("interaction_", year_group)
  
  df <- df %>% 
    mutate(
      !!indicator_name := ifelse(year_group == !!year_group, 1, 0),
      !!interaction_name := !!sym(indicator_name) * FATALITIES_DUMMY
    )
}

# Create formula for models (excluding earliest year from interactions)
interaction_terms <- paste0(
  "interaction_", 
  year_groups[year_groups != earliest_year], 
  collapse = " + "
)

# Run four progressive regression models
model1 <- feols(as.formula(paste("change_2006_2016 ~", interaction_terms)), data = df)

model2 <- feols(as.formula(paste("change_2006_2016 ~", interaction_terms, "+ simple_family")), data = df)

model3 <- feols(as.formula(paste("change_2006_2016 ~", interaction_terms, "+ simple_family + Region")), data = df)

model4 <- feols(as.formula(paste("change_2006_2016 ~", interaction_terms, "+ simple_family + STATE")), data = df)

# Generate summary table of all models
stargazer(model1, model2, model3, model4, 
          type = "html", 
          title = "Event Study Model Results", 
          column.labels = c("Original Model", 
                            "Model with Zoning Categories", 
                            "Model with Zoning Categories and Region", 
                            "Model with Zoning Categories and State"), 
          out = "C:/Users/clint/Desktop/Research-Paper-Trucks/Exploration/5/model_summary_combined.html")

# Extract and process coefficients for plotting

# Process each model's coefficients
process_interaction_coefs <- function(model, term_pattern) {
  tidy(model) %>%
    filter(grepl(term_pattern, term)) %>%
    mutate(year_group = as.numeric(gsub(term_pattern, "", term)))
}

# Model 1: Extract interaction coefficients
coef_model1 <- process_interaction_coefs(model1, "interaction_")

# Model 2: Extract coefficients
coef_model2 <- tidy(model2)
int_coef_model2 <- process_interaction_coefs(model2, "interaction_")
sf_coef_model2 <- coef_model2 %>% 
  filter(grepl("simple_family", term)) %>%
  mutate(simple_family = gsub("simple_family", "", term))

# Model 3: Extract coefficients
coef_model3 <- tidy(model3)
int_coef_model3 <- process_interaction_coefs(model3, "interaction_")
sf_coef_model3 <- coef_model3 %>% 
  filter(grepl("simple_family", term)) %>%
  mutate(simple_family = gsub("simple_family", "", term))
region_coef_model3 <- coef_model3 %>% 
  filter(grepl("Region", term)) %>%
  mutate(Region = gsub("Region", "", term))

# Model 4: Extract coefficients
coef_model4 <- tidy(model4)
int_coef_model4 <- process_interaction_coefs(model4, "interaction_")
sf_coef_model4 <- coef_model4 %>% 
  filter(grepl("simple_family", term)) %>%
  mutate(simple_family = gsub("simple_family", "", term))
state_coef_model4 <- coef_model4 %>% 
  filter(grepl("STATE", term)) %>%
  mutate(STATE = gsub("STATE", "", term))

# Function for creating coefficient plots
plot_coefs <- function(data, x_var, title, x_lab, subtitle = NULL) {
  p <- ggplot(data, aes(x = !!sym(x_var), y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
    labs(title = title,
         x = x_lab,
         y = "Coefficient Estimate") +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red")
  
  if (!is.null(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }
  
  return(p)
}

# Create plots
# Model 1 plot
plot1 <- plot_coefs(
  coef_model1, 
  "year_group", 
  "Coefficients of Fatality * Year Terms VS Change in Truck Stops (Original Model)",
  "Year Group",
  paste("Reference Year:", earliest_year)
)

# Model 2 plots
plot2a <- plot_coefs(
  int_coef_model2, 
  "year_group", 
  "Coefficients of Fatality * Year Terms VS Change in Truck Stops (Model with Zoning Categories)",
  "Year Group",
  paste("Reference Year:", earliest_year)
)

plot2b <- plot_coefs(
  sf_coef_model2, 
  "simple_family", 
  "Coefficients of Zoning Category VS Change in Truck Stops (Model with Zoning Categories)",
  "Zoning Category"
)

# Model 3 plots
plot3a <- plot_coefs(
  int_coef_model3, 
  "year_group", 
  "Coefficients of Fatality * Year Terms VS Change in Truck Stops (Model with Zoning Categories and Region)",
  "Year Group",
  paste("Reference Year:", earliest_year)
)

plot3b <- plot_coefs(
  sf_coef_model3, 
  "simple_family", 
  "Coefficients of Zoning Category VS Change in Truck Stops (Model with Zoning Categories and Region)",
  "Zoning Category"
)

plot3c <- plot_coefs(
  region_coef_model3, 
  "Region", 
  "Coefficients of Region VS Change in Truck Stops (Model with Zoning Categories and Region)",
  "Region"
)

# Model 4 plots
plot4a <- plot_coefs(
  int_coef_model4, 
  "year_group", 
  "Coefficients of Fatality * Year Terms VS Change in Truck Stops (Model with Zoning Categories and State)",
  "Year Group",
  paste("Reference Year:", earliest_year)
)

plot4b <- plot_coefs(
  sf_coef_model4, 
  "simple_family", 
  "Coefficients of Zoning Category VS Change in Truck Stops (Model with Zoning Categories and State)",
  "Zoning Category"
)

plot4c <- plot_coefs(
  state_coef_model4, 
  "STATE", 
  "Coefficients of STATE VS Change in Truck Stops (Model with Zoning Categories and State)",
  "STATE"
)

# Display plots
print(plot1)
print(plot2a)
print(plot2b)
print(plot3a)
print(plot3b)
print(plot3c)
print(plot4a)
print(plot4b)
print(plot4c)