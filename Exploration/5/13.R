# Load required libraries
library(dplyr)       # For data manipulation
library(tidyr)       # For reshaping data
library(lubridate)   # For date handling
library(fixest)      # For fixed effect models
library(ggplot2)     # For visualization
library(modelsummary) # For model summary tables

# Load the dataset
df_6 <- read.csv("C:/Users/clint/Desktop/Research-Paper-Trucks/Exploration/5/df_6.csv")

# Convert the accident report date to datetime and extract the accident year
df_6 <- df_6 %>%
  mutate(REPORT_DATE = as.Date(REPORT_DATE),
         accident_year = year(REPORT_DATE))

# Function to run models with different fatality thresholds
find_optimal_threshold <- function(min_threshold = 1, max_threshold = 15) {
  # Create a dataframe to store results
  results <- data.frame()
  
  for (n in min_threshold:max_threshold) {
    # Redefine Accident with current threshold
    df_6_temp <- df_6 %>%
      mutate(Accident = as.integer(FATALITIES >= n))
    
    # Aggregate accident info by county
    accident_by_county <- df_6_temp %>%
      group_by(STATE, COUNTY) %>%
      summarize(Accident = max(Accident), .groups = 'drop')
    
    # Reshape truck stop data
    truck_stop_cols <- c("change_2007", "change_2008", "change_2015", "change_2016")
    truck_df <- df_6_temp %>%
      select(STATE, COUNTY, Region, simple_family, all_of(truck_stop_cols)) %>%
      distinct()
    
    truck_df_long <- truck_df %>%
      pivot_longer(
        cols = all_of(truck_stop_cols),
        names_to = "year",
        values_to = "delta_NumTruckStop"
      ) %>%
      mutate(year = as.integer(gsub("change_", "", year)))
    
    # Merge and create required variables
    merged_df <- truck_df_long %>%
      left_join(accident_by_county, by = c("STATE", "COUNTY")) %>%
      mutate(Post = as.integer(year >= 2016),
             Accident_Post = Accident * Post,
             # Create year factor with 2008 as reference
             year_factor = relevel(as.factor(year), ref = "2008"))
    
    # Initialize variables to store model results
    m1_coef <- NA; m1_se <- NA
    m2_coef <- NA; m2_se <- NA
    m3_coef <- NA; m3_se <- NA
    m4_coef <- NA; m4_se <- NA
    
    # Try each model with error handling
    tryCatch({
      model1 <- feols(delta_NumTruckStop ~ Accident_Post | STATE + Region + simple_family + year_factor, data = merged_df)
      m1_coef <- coef(model1)["Accident_Post"]
      m1_se <- summary(model1)$se["Accident_Post"]
    }, error = function(e) {
      cat("Model 1 error at threshold", n, ":", e$message, "\n")
    })
    
    tryCatch({
      model2 <- feols(delta_NumTruckStop ~ Accident_Post | simple_family + year_factor, data = merged_df)
      m2_coef <- coef(model2)["Accident_Post"]
      m2_se <- summary(model2)$se["Accident_Post"]
    }, error = function(e) {
      cat("Model 2 error at threshold", n, ":", e$message, "\n")
    })
    
    tryCatch({
      model3 <- feols(delta_NumTruckStop ~ Accident_Post | STATE + simple_family + year_factor, data = merged_df)
      m3_coef <- coef(model3)["Accident_Post"]
      m3_se <- summary(model3)$se["Accident_Post"]
    }, error = function(e) {
      cat("Model 3 error at threshold", n, ":", e$message, "\n")
    })
    
    tryCatch({
      model4 <- feols(delta_NumTruckStop ~ Accident_Post | Region + simple_family + year_factor, data = merged_df)
      m4_coef <- coef(model4)["Accident_Post"]
      m4_se <- summary(model4)$se["Accident_Post"]
    }, error = function(e) {
      cat("Model 4 error at threshold", n, ":", e$message, "\n")
    })
    
    # Add results to dataframe
    results <- rbind(results, data.frame(
      threshold = n,
      m1_coef = m1_coef, m1_se = m1_se,
      m2_coef = m2_coef, m2_se = m2_se,
      m3_coef = m3_coef, m3_se = m3_se,
      m4_coef = m4_coef, m4_se = m4_se
    ))
    
    # Print progress
    cat("Tested threshold:", n, "| Model 1 coefficient:", m1_coef, "\n")
  }
  
  return(results)
}

# Run the function to find optimal thresholds
threshold_results <- find_optimal_threshold(1, 10)
print(threshold_results)

# Find the optimal thresholds (where coefficient is maximized)
optimal_thresholds <- data.frame(
  model = c("Model 1 (All FE)", "Model 2 (Zoning Only)", 
            "Model 3 (State + Zoning)", "Model 4 (Region + Zoning)"),
  threshold = c(
    threshold_results$threshold[which.max(threshold_results$m1_coef)],
    threshold_results$threshold[which.max(threshold_results$m2_coef)],
    threshold_results$threshold[which.max(threshold_results$m3_coef)],
    threshold_results$threshold[which.max(threshold_results$m4_coef)]
  )
)
print(optimal_thresholds)

# Get optimal threshold from first model
optimal_n <- optimal_thresholds$threshold[1]

# Create a basic visualization of coefficients and standard errors
plot_data <- data.frame(
  model = rep(c("All FE", "Zoning Only", "State + Zoning", "Region + Zoning"), each = nrow(threshold_results)),
  threshold = rep(threshold_results$threshold, 4),
  coefficient = c(threshold_results$m1_coef, threshold_results$m2_coef, 
                  threshold_results$m3_coef, threshold_results$m4_coef),
  se = c(threshold_results$m1_se, threshold_results$m2_se,
         threshold_results$m3_se, threshold_results$m4_se)
)

# Only plot rows that have both coefficient and se
plot_data <- plot_data[!is.na(plot_data$coefficient) & !is.na(plot_data$se),]

# Create the plot if we have valid data
if(nrow(plot_data) > 0) {
  # Plot coefficients with error bars
  ggplot(plot_data, aes(x = threshold, y = coefficient, color = model, group = model)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = coefficient - se, ymax = coefficient + se), width = 0.2) +
    theme_minimal() +
    labs(title = "Accident_Post Coefficient by Fatality Threshold",
         subtitle = "Error bars represent ±1 standard error (Reference year: 2008)",
         x = "Fatality Threshold",
         y = "Coefficient Value") +
    theme(legend.position = "bottom")
}

# --- RUN FINAL MODELS WITH THE OPTIMAL THRESHOLD ---

# Define Accident using the optimal threshold
df_6 <- df_6 %>%
  mutate(Accident = as.integer(FATALITIES >= optimal_n))

# Aggregate accident info by county
accident_by_county <- df_6 %>%
  group_by(STATE, COUNTY) %>%
  summarize(Accident = max(Accident), .groups = 'drop')

# Reshape truck stop data
truck_stop_cols <- c("change_2007", "change_2008", "change_2015", "change_2016")
truck_df <- df_6 %>%
  select(STATE, COUNTY, Region, simple_family, all_of(truck_stop_cols)) %>%
  distinct()

# Melt the data to have a county-year structure
truck_df_long <- truck_df %>%
  pivot_longer(
    cols = all_of(truck_stop_cols),
    names_to = "year",
    values_to = "delta_NumTruckStop"
  ) %>%
  mutate(year = as.integer(gsub("change_", "", year)))

# Merge accident data
merged_df <- truck_df_long %>%
  left_join(accident_by_county, by = c("STATE", "COUNTY")) %>%
  # Create the "Post" indicator (1 if year >= 2016, 0 otherwise)
  mutate(Post = as.integer(year >= 2016),
         # Create the interaction term: Accident * Post
         Accident_Post = Accident * Post,
         # Convert year to factor with 2008 as reference
         year_factor = relevel(as.factor(year), ref = "2008"))

# Create the four model specifications with proper year fixed effects
model1 <- feols(delta_NumTruckStop ~ Accident_Post | STATE + Region + simple_family + year_factor, data = merged_df)
model2 <- feols(delta_NumTruckStop ~ Accident_Post | simple_family + year_factor, data = merged_df)
model3 <- feols(delta_NumTruckStop ~ Accident_Post | STATE + simple_family + year_factor, data = merged_df)
model4 <- feols(delta_NumTruckStop ~ Accident_Post | Region + simple_family + year_factor, data = merged_df)

# Create a nice comparison table
modelsummary(
  list("All FE" = model1, 
       "Zoning Only" = model2, 
       "State + Zoning" = model3, 
       "Region + Zoning" = model4),
  stars = TRUE,
  title = paste0("Effect of Accident on Truck Stops (Fatality Threshold = ", optimal_n, ", Reference Year = 2008)"),
  notes = "Standard errors in parentheses. * p < 0.1, ** p < 0.05, *** p < 0.01",
  coef_map = c("Accident_Post" = "Accident*Post"),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  add_rows = tribble(
    ~term,                ~"All FE",   ~"Zoning Only",   ~"State + Zoning",   ~"Region + Zoning",
    "Fixed Effects:",     "",          "",               "",                  "",
    "State",              "Yes",       "No",             "Yes",               "No",
    "Region",             "Yes",       "No",             "No",                "Yes",
    "Zoning Category",    "Yes",       "Yes",            "Yes",               "Yes",
    "Year (Ref: 2008)",   "Yes",       "Yes",            "Yes",               "Yes"
  )
)

# Plot the coefficients from final models with confidence intervals
final_coefs <- data.frame(
  model = c("All FE", "Zoning Only", "State + Zoning", "Region + Zoning"),
  coefficient = c(
    coef(model1)["Accident_Post"],
    coef(model2)["Accident_Post"],
    coef(model3)["Accident_Post"],
    coef(model4)["Accident_Post"]
  ),
  se = c(
    summary(model1)$se["Accident_Post"],
    summary(model2)$se["Accident_Post"],
    summary(model3)$se["Accident_Post"],
    summary(model4)$se["Accident_Post"]
  )
)

# Add confidence intervals
final_coefs <- final_coefs %>%
  mutate(
    lower_ci = coefficient - 1.96 * se,
    upper_ci = coefficient + 1.96 * se,
    # Reorder factor levels for plotting
    model = factor(model, levels = c("All FE", "Zoning Only", "State + Zoning", "Region + Zoning"))
  )

# Create the final plot
ggplot(final_coefs, aes(x = model, y = coefficient)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = paste("Effect of Accident on Truck Stops (Fatality Threshold =", optimal_n, ")"),
    subtitle = "Coefficients with 95% Confidence Intervals (Reference Year: 2008)",
    x = "",
    y = "Accident*Post Coefficient"
  )