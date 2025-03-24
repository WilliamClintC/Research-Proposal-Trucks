# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(fixest)
library(ggplot2)
library(modelsummary)

df_6 <- read.csv("C:/Users/clint/Desktop/Research-Paper-Trucks/Exploration/5/df_6.csv")
optimal_n=1
# Data preparation (assuming df_6 is loaded)
df_6 <- df_6 %>%
  mutate(REPORT_DATE = as.Date(REPORT_DATE),
         accident_year = year(REPORT_DATE),
         Accident = as.integer(FATALITIES >= optimal_n)) # assuming optimal_n is defined

# Aggregate accident info by county
accident_by_county <- df_6 %>%
  group_by(STATE, COUNTY) %>%
  summarize(Accident = max(Accident), .groups = 'drop')

# Reshape truck stop data
truck_stop_cols <- c("change_2007", "change_2008", "change_2015", "change_2016")
truck_df <- df_6 %>%
  select(STATE, COUNTY, Region, simple_family, all_of(truck_stop_cols)) %>%
  distinct()

# Create panel data structure
truck_df_long <- truck_df %>%
  pivot_longer(
    cols = all_of(truck_stop_cols),
    names_to = "year",
    values_to = "delta_NumTruckStop"
  ) %>%
  mutate(year = as.integer(gsub("change_", "", year)))

# Merge accident data and create DiD variables
merged_df <- truck_df_long %>%
  left_join(accident_by_county, by = c("STATE", "COUNTY")) %>%
  # Create year indicators instead of a single Post variable (with 2008 as reference)
  mutate(
    y2007 = as.integer(year == 2007),
    y2015 = as.integer(year == 2015),
    y2016 = as.integer(year == 2016)
  )

# Run enhanced DiD models with separate interactions for each year
model1 <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016 | 
                  STATE + Region + simple_family, data = merged_df)

model2 <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016 | 
                  simple_family, data = merged_df)

model3 <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016 | 
                  STATE + simple_family, data = merged_df)

model4 <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016 | 
                  Region + simple_family, data = merged_df)

# --- VISUALIZATION 1: MODEL SUMMARY TABLE ---
modelsummary(
  list("Model 1: Year FE" = model1, 
       "Model 2: Year + Zoning FE" = model2, 
       "Model 3: Year + Region + Zoning FE" = model3, 
       "Model 4: Year + State + Zoning FE" = model4),
  stars = TRUE,
  title = paste0("Effect of Accident on Truck Stops (Reference Year = 2008)"),
  notes = "Standard errors in parentheses. * p < 0.1, ** p < 0.05, *** p < 0.01",
  coef_map = c(
    "Accident:y2007" = "Accident × 2007",
    "Accident:y2015" = "Accident × 2015",
    "Accident:y2016" = "Accident × 2016",
    "y2007" = "Year 2007",
    "y2015" = "Year 2015",
    "y2016" = "Year 2016",
    "Accident" = "Accident"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  add_rows = tribble(
    ~term,                ~"Model 1: Year FE",   ~"Model 2: Year + Zoning FE",   ~"Model 3: Year + Region + Zoning FE",   ~"Model 4: Year + State + Zoning FE",
    "Fixed Effects:",     "",                   "",                            "",                                      "",
    "State",              "Yes",                "No",                          "Yes",                                   "No",
    "Region",             "Yes",                "No",                          "No",                                    "Yes",
    "Zoning Category",    "Yes",                "Yes",                         "Yes",                                   "Yes"
  )
)

# --- VISUALIZATION 2: EVENT STUDY PLOT ---
# Extract coefficients and standard errors for interaction terms
event_study_data <- data.frame(
  model = rep(c("Model 1: Year FE", "Model 2: Year + Zoning FE", "Model 3: Year + Region + Zoning FE", "Model 4: Year + State + Zoning FE"), each = 3),
  year = rep(c(2007, 2015, 2016), 4),
  coefficient = c(
    coef(model1)["Accident:y2007"], coef(model1)["Accident:y2015"], coef(model1)["Accident:y2016"],
    coef(model2)["Accident:y2007"], coef(model2)["Accident:y2015"], coef(model2)["Accident:y2016"],
    coef(model3)["Accident:y2007"], coef(model3)["Accident:y2015"], coef(model3)["Accident:y2016"],
    coef(model4)["Accident:y2007"], coef(model4)["Accident:y2015"], coef(model4)["Accident:y2016"]
  ),
  se = c(
    summary(model1)$se["Accident:y2007"], summary(model1)$se["Accident:y2015"], summary(model1)$se["Accident:y2016"],
    summary(model2)$se["Accident:y2007"], summary(model2)$se["Accident:y2015"], summary(model2)$se["Accident:y2016"],
    summary(model3)$se["Accident:y2007"], summary(model3)$se["Accident:y2015"], summary(model3)$se["Accident:y2016"],
    summary(model4)$se["Accident:y2007"], summary(model4)$se["Accident:y2015"], summary(model4)$se["Accident:y2016"]
  )
)

# Add confidence intervals
event_study_data <- event_study_data %>%
  mutate(
    lower_ci = coefficient - 1.96 * se,
    upper_ci = coefficient + 1.96 * se,
    # Add reference year (2008) with zero coefficient
    model = factor(model, levels = c("Model 1: Year FE", "Model 2: Year + Zoning FE", "Model 3: Year + Region + Zoning FE", "Model 4: Year + State + Zoning FE"))
  )

# Create event study plot
ggplot(event_study_data, aes(x = year, y = coefficient, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                position = position_dodge(width = 0.5), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2008, linetype = "dotted", color = "gray") +
  # Add a point for reference year
  geom_point(data = data.frame(
    year = rep(2008, 4),
    coefficient = rep(0, 4),
    model = factor(c("Model 1: Year FE", "Model 2: Year + Zoning FE", "Model 3: Year + Region + Zoning FE", "Model 4: Year + State + Zoning FE"),
                   levels = c("Model 1: Year FE", "Model 2: Year + Zoning FE", "Model 3: Year + Region + Zoning FE", "Model 4: Year + State + Zoning FE"))
  ), aes(x = year, y = coefficient, color = model, shape = model), size = 3) +
  scale_x_continuous(breaks = c(2007, 2008, 2015, 2016)) +
  theme_minimal() +
  labs(
    title = "Effect of High-Fatality Crashes on Truck Stop Construction",
    subtitle = "Reference year: 2008 - All models include year fixed effects",
    x = "Year Period",
    y = "Coefficient (Change in Truck Stop Construction)",
    color = "Model Specification",
    shape = "Model Specification"
  ) +
  theme(legend.position = "bottom")



#part last 
# First, check if the model has the simple_family fixed effect
has_zoning_fe <- function(model) {
  fe_names <- names(fixef(model))
  return("simple_family" %in% fe_names)
}

# Create a safe extraction function that checks if the fixed effect exists
extract_zoning_fe <- function(model, model_name) {
  if (!has_zoning_fe(model)) {
    message("No simple_family fixed effect in ", model_name)
    return(NULL)
  }
  
  # Get fixed effects for simple_family
  fe <- fixef(model)$simple_family
  
  # Extract standard errors if available
  se_attr <- attr(fe, "se")
  if (is.null(se_attr)) {
    message("No standard errors found for ", model_name)
    se <- rep(NA, length(fe))
  } else {
    se <- se_attr
  }
  
  # Create a data frame
  result <- data.frame(
    category = names(fe),
    estimate = as.numeric(fe),
    se = as.numeric(se),
    model = model_name,
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# Try to extract fixed effects from each model
fe_df1 <- extract_zoning_fe(model1, "Model 1: Year FE")
fe_df2 <- extract_zoning_fe(model2, "Model 2: Year + Zoning FE")
fe_df3 <- extract_zoning_fe(model3, "Model 3: Year + Region + Zoning FE")
fe_df4 <- extract_zoning_fe(model4, "Model 4: Year + State + Zoning FE")

# Combine all valid fixed effects
fe_list <- list(fe_df1, fe_df2, fe_df3, fe_df4)
valid_fe <- fe_list[!sapply(fe_list, is.null)]

if (length(valid_fe) == 0) {
  stop("No valid zoning fixed effects found in any model")
}

all_fe <- bind_rows(valid_fe)

# Print diagnostic information
print(paste("Total zoning fixed effects collected:", nrow(all_fe)))
print(paste("Number of unique categories:", length(unique(all_fe$category))))

# Calculate average effect size for each category to order them
category_avg <- all_fe %>%
  group_by(category) %>%
  summarize(avg_effect = mean(estimate, na.rm = TRUE)) %>%
  arrange(avg_effect)

# Order categories by average effect
all_fe$category <- factor(all_fe$category, levels = category_avg$category)
# Create a plot with categories on the x-axis
ggplot(all_fe, aes(x = category, y = estimate, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96*se, 
                    ymax = estimate + 1.96*se),
                position = position_dodge(width = 0.8), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # No coord_flip() here to keep categories on x-axis
  theme_minimal() +
  labs(
    title = "Impact of Zoning Categories on Truck Stop Construction",
    subtitle = "Comparison across model specifications",
    y = "Coefficient (Effect on Truck Stop Construction)",
    x = "Zoning Category",
    color = "Model Specification",
    shape = "Model Specification"
  ) +
  theme(
    legend.position = "bottom",
    # Adjust x-axis text for better readability
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(nrow = 2))



# part last last





# After calculating the average effect for each category
category_avg <- all_fe %>%
  group_by(category) %>%
  summarize(avg_effect = mean(estimate, na.rm = TRUE)) %>%
  arrange(avg_effect)

# Find the midpoint category (closest to median effect)
median_effect <- median(category_avg$avg_effect)
midpoint_idx <- which.min(abs(category_avg$avg_effect - median_effect))
midpoint_category <- category_avg$category[midpoint_idx]

# Print information about the reference category
print(paste("Using midpoint category as reference:", midpoint_category))
print(paste("Midpoint category average effect:", 
            round(category_avg$avg_effect[midpoint_idx], 4)))

# Adjust all estimates relative to the midpoint category
all_fe_adjusted <- all_fe %>%
  group_by(model) %>%
  mutate(
    # Find the estimate for the midpoint category in this model
    reference_value = estimate[category == midpoint_category],
    # Adjust all estimates relative to the reference
    adjusted_estimate = estimate - reference_value,
    # Standard errors remain the same
    adjusted_se = se
  ) %>%
  ungroup()

# Calculate confidence intervals explicitly
all_fe_adjusted <- all_fe_adjusted %>%
  mutate(
    lower_ci = adjusted_estimate - 1.96 * adjusted_se,
    upper_ci = adjusted_estimate + 1.96 * adjusted_se
  )

# Order categories by adjusted effect
adjusted_category_avg <- all_fe_adjusted %>%
  group_by(category) %>%
  summarize(avg_effect = mean(adjusted_estimate, na.rm = TRUE)) %>%
  arrange(avg_effect)

# Set factor levels for proper ordering
all_fe_adjusted$category <- factor(all_fe_adjusted$category, 
                                   levels = adjusted_category_avg$category)

# Create the adjusted plot with clearer confidence intervals
ggplot(all_fe_adjusted, aes(x = category, y = adjusted_estimate, color = model, shape = model)) +
  # Add a confidence interval ribbon with transparency
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 0.8), 
                width = 0.3,
                linewidth = 0.8,
                alpha = 0.8) +
  # Add the point estimates
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  # Zero reference line
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Impact of Zoning Categories on Truck Stop Construction",
    subtitle = paste("Reference category (0):", midpoint_category),
    y = "Relative Effect on Truck Stop Construction",
    x = "Zoning Category",
    color = "Model Specification",
    shape = "Model Specification",
    caption = "Error bars represent 95% confidence intervals"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(hjust = 0, face = "italic")
  ) +
  guides(color = guide_legend(nrow = 2))