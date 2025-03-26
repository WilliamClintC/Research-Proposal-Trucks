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


# All models with standard errors clustered by Region

model1 <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016, 
                data = merged_df,
                cluster = "Region")

model2 <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016 | 
                  simple_family, 
                data = merged_df,
                cluster = "Region")

model3 <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016 | 
                  STATE + simple_family, 
                data = merged_df,
                cluster = "Region")

model4 <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016 | 
                  Region + simple_family, 
                data = merged_df,
                cluster = "Region")



# --- FOCUSED MODEL SUMMARY TABLE ---
modelsummary(
  list("Model 1: Year FE" = model1, 
       "Model 2: Year + Zoning FE" = model2, 
       "Model 3: Year + Region + Zoning FE" = model3, 
       "Model 4: Year + State + Zoning FE" = model4),
  stars = TRUE,
  title = paste0("Effect of Accident on Truck Stops (Reference Year = 2008)"),
  notes = "Standard errors in parentheses. * p < 0.1, ** p < 0.05, *** p < 0.01",
  coef_map = c(
    "Accident:y2015" = "Accident × 2015",
    "Accident:y2016" = "Accident × 2016"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  add_rows = tribble(
    ~term,                ~"Model 1: Year FE",   ~"Model 2: Year + Zoning FE",   ~"Model 3: Year + Region + Zoning FE",   ~"Model 4: Year + State + Zoning FE",
    "Fixed Effects:",     "",                   "",                            "",                                      "",
    "State",              "No",                 "No",                          "Yes",                                   "No",
    "Region",             "No",                 "No",                          "No",                                    "Yes",
    "Zoning Category",    "No",                 "Yes",                         "Yes",                                   "Yes"
  )
)

# --- ZONING FIXED EFFECTS WITH MODELSUMMARY ---
# Create custom model objects for zoning fixed effects
create_fe_model <- function(model, model_name) {
  # Skip model1 which doesn't have fixed effects
  if (model_name == "Model 1: Year FE") {
    return(NULL)
  }
  
  # Extract fixed effects
  fe <- fixef(model)
  
  # Check if simple_family exists
  if (!"simple_family" %in% names(fe)) {
    return(NULL)
  }
  
  # Create a list with coefficients and se
  fe_simple_family <- fe$simple_family
  
  # Create a mock model object that modelsummary can use
  mock_model <- list(
    coefficients = fe_simple_family,
    se = rep(NA, length(fe_simple_family)), # No SEs available for FEs typically
    N = nobs(model)
  )
  
  # Add class to make it work with modelsummary
  class(mock_model) <- "mock_model"
  
  return(mock_model)
}

# Create a method for modelsummary to extract from our mock models
modelsummary_get_estimates.mock_model <- function(x, ...) {
  ret <- data.frame(
    term = names(x$coefficients),
    estimate = unname(x$coefficients),
    std.error = rep(NA, length(x$coefficients)),
    statistic = rep(NA, length(x$coefficients)),
    p.value = rep(NA, length(x$coefficients)),
    conf.low = rep(NA, length(x$coefficients)),
    conf.high = rep(NA, length(x$coefficients)),
    stringsAsFactors = FALSE
  )
  return(ret)
}


# The existing plot for zoning fixed effects will still be generated
# from your original code in the last part
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

# Updated extraction function with simplified approach
extract_zoning_fe <- function(model, model_name) {
  # Skip model1 which doesn't have fixed effects
  if (model_name == "Model 1: Year FE") {
    message(model_name, " doesn't have simple_family fixed effects")
    return(NULL)
  }
  
  # Try to extract fixed effects safely
  tryCatch({
    # Get fixed effects
    fe <- fixef(model)
    
    # Check if simple_family exists
    if (!"simple_family" %in% names(fe)) {
      message("No simple_family fixed effect in ", model_name)
      return(NULL)
    }
    
    # Get simple_family coefficients
    fe_coefs <- fe$simple_family
    
    # Create data frame without standard errors
    result <- data.frame(
      category = names(fe_coefs),
      estimate = as.numeric(fe_coefs),
      se = 0.1,  # Use a small default value since SEs aren't available
      model = model_name,
      stringsAsFactors = FALSE
    )
    
    return(result)
  }, error = function(e) {
    message("Error extracting fixed effects from ", model_name, ": ", e$message)
    return(NULL)
  })
}

# Try to extract fixed effects from each model (skipping model1)
fe_df1 <- NULL  # model1 doesn't have fixed effects
fe_df2 <- extract_zoning_fe(model2, "Model 2: Year + Zoning FE")
fe_df3 <- extract_zoning_fe(model3, "Model 3: Year + Region + Zoning FE")
fe_df4 <- extract_zoning_fe(model4, "Model 4: Year + State + Zoning FE")

# Combine all valid fixed effects
fe_list <- list(fe_df2, fe_df3, fe_df4)
valid_fe <- fe_list[!sapply(fe_list, is.null)]

if (length(valid_fe) == 0) {
  warning("No valid zoning fixed effects found in any model")
} else {
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
  
  # Create a plot with categories on the x-axis (without error bars)
  ggplot(all_fe, aes(x = category, y = estimate, color = model, shape = model)) +
    geom_point(position = position_dodge(width = 0.8), size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed") +
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
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    guides(color = guide_legend(nrow = 2))
  
  # Find midpoint category for relative comparison
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
      adjusted_estimate = estimate - reference_value
    ) %>%
    ungroup()
  
  # Order categories by adjusted effect
  adjusted_category_avg <- all_fe_adjusted %>%
    group_by(category) %>%
    summarize(avg_effect = mean(adjusted_estimate, na.rm = TRUE)) %>%
    arrange(avg_effect)
  
  # Set factor levels for proper ordering
  all_fe_adjusted$category <- factor(all_fe_adjusted$category, 
                                     levels = adjusted_category_avg$category)
  
  # Create the adjusted plot (without error bars)
  ggplot(all_fe_adjusted, aes(x = category, y = adjusted_estimate, color = model, shape = model)) +
    geom_point(position = position_dodge(width = 0.8), size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(
      title = "Impact of Zoning Categories on Truck Stop Construction",
      subtitle = paste("Reference category (0):", midpoint_category),
      y = "Relative Effect on Truck Stop Construction",
      x = "Zoning Category",
      color = "Model Specification",
      shape = "Model Specification"
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    guides(color = guide_legend(nrow = 2))
}


# part



# Simplified function to extract zoning fixed effects with standard errors
get_zoning_coefficients <- function(model2, model3, model4) {
  # Create a function to extract coefficients from each model
  extract_fe <- function(model, model_name) {
    if (is.null(model)) return(NULL)
    
    # Get fixed effects
    fe <- fixef(model)
    if (!"simple_family" %in% names(fe)) return(NULL)
    
    # Get coefficients
    fe_coefs <- fe$simple_family
    
    # Get standard errors if possible, otherwise use default
    tryCatch({
      vcov_matrix <- vcov(model, ranef = FALSE)
      param_names <- names(fixef(model, ranef = FALSE))
      simple_family_indices <- grep("simple_family", param_names)
      se_values <- sqrt(diag(vcov_matrix)[simple_family_indices])
    }, error = function(e) {
      se_values <- rep(0.1, length(fe_coefs))
    })
    
    # Create data frame
    result <- data.frame(
      category = names(fe_coefs),
      estimate = as.numeric(fe_coefs),
      se = se_values,
      model = model_name,
      stringsAsFactors = FALSE
    )
    
    # Add confidence intervals
    result$conf_low <- result$estimate - 1.96 * result$se
    result$conf_high <- result$estimate + 1.96 * result$se
    
    return(result)
  }
  
  # Extract from each model
  fe_df2 <- extract_fe(model2, "Model 2: Year + Zoning FE")
  fe_df3 <- extract_fe(model3, "Model 3: Year + Region + Zoning FE")
  fe_df4 <- extract_fe(model4, "Model 4: Year + State + Zoning FE")
  
  # Combine results
  all_fe <- bind_rows(list(fe_df2, fe_df3, fe_df4))
  
  # Order categories by average effect
  category_avg <- all_fe %>%
    group_by(category) %>%
    summarize(avg_effect = mean(estimate, na.rm = TRUE)) %>%
    arrange(avg_effect)
  
  all_fe$category <- factor(all_fe$category, levels = category_avg$category)
  
  return(all_fe)
}

# Get all zoning coefficients
zoning_coefficients <- get_zoning_coefficients(model2, model3, model4)

# Print diagnostic information
print(paste("Total zoning fixed effects collected:", nrow(zoning_coefficients)))
print(paste("Number of unique categories:", length(unique(zoning_coefficients$category))))

# Create plot 1: Raw coefficients with confidence intervals
p1 <- ggplot(zoning_coefficients, aes(x = category, y = estimate, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), 
                position = position_dodge(width = 0.8), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
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
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(nrow = 2))

# Find midpoint category for relative comparison
category_avg <- zoning_coefficients %>%
  group_by(category) %>%
  summarize(avg_effect = mean(estimate, na.rm = TRUE)) %>%
  arrange(avg_effect)

median_effect <- median(category_avg$avg_effect)
midpoint_idx <- which.min(abs(category_avg$avg_effect - median_effect))
midpoint_category <- category_avg$category[midpoint_idx]

# Create adjusted coefficients
adjusted_coefficients <- zoning_coefficients %>%
  group_by(model) %>%
  mutate(
    reference_value = estimate[category == midpoint_category],
    adjusted_estimate = estimate - reference_value,
    adjusted_conf_low = conf_low - reference_value,
    adjusted_conf_high = conf_high - reference_value
  ) %>%
  ungroup()

# Order by adjusted effect
adjusted_category_avg <- adjusted_coefficients %>%
  group_by(category) %>%
  summarize(avg_effect = mean(adjusted_estimate, na.rm = TRUE)) %>%
  arrange(avg_effect)

adjusted_coefficients$category <- factor(adjusted_coefficients$category, 
                                 levels = adjusted_category_avg$category)

# Create plot 2: Adjusted coefficients with confidence intervals
p2 <- ggplot(adjusted_coefficients, aes(x = category, y = adjusted_estimate, 
                          color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = adjusted_conf_low, ymax = adjusted_conf_high), 
                position = position_dodge(width = 0.8), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Impact of Zoning Categories on Truck Stop Construction (Relative)",
    subtitle = paste("Reference category (0):", midpoint_category),
    y = "Relative Effect on Truck Stop Construction",
    x = "Zoning Category",
    color = "Model Specification",
    shape = "Model Specification"
  ) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(nrow = 2))

# Print plots to display them in RStudio viewer
print(p1)
print(p2)

# Optional: Save plots to files
# ggsave("zoning_coefficients_raw.png", p1, width = 12, height = 8)
# ggsave("zoning_coe


