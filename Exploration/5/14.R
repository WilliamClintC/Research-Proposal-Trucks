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
model1 <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016, 
                data = merged_df)

model2 <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016 | 
                  simple_family, data = merged_df)

model3 <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016 | 
                  STATE + simple_family, data = merged_df)

model4 <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016 | 
                  Region + simple_family, data = merged_df)



# --- FOCUSED MODEL SUMMARY TABLE ---
modelsummary(
  list("Model 1: Year FE" = model1, 
       "Model 2: Year + Zoning FE" = model2, 
       "Model 3: Year + Region + Zoning FE" = model3, 
       "Model 4: Year + State + Zoning FE" = model4),
  stars = TRUE,
  fixef = TRUE,
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

#part 
# Create a copy of the data with releveled simple_family
merged_df_mod <- merged_df
merged_df_mod$simple_family <- relevel(as.factor(merged_df_mod$simple_family), ref = "Wild Texas")

# Create alternative models with simple_family as regular variables
model2_alt <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016 + 
                      simple_family, data = merged_df_mod)

model3_alt <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016 + 
                      simple_family | STATE, data = merged_df_mod)

model4_alt <- feols(delta_NumTruckStop ~ Accident*y2007 + Accident*y2015 + Accident*y2016 + 
                      simple_family | Region, data = merged_df_mod)

# Get model coefficients names
all_coefs <- names(coef(model2_alt))
family_coefs <- all_coefs[grep("simple_family", all_coefs)]

# Create a clean coefficient mapping
clean_names <- gsub("simple_family", "", family_coefs)
coef_mapping <- setNames(clean_names, family_coefs)

# Create a professional table with clean category names
modelsummary(
  list("Model 2: Year + Zoning" = model2_alt, 
       "Model 3: Year + State + Zoning" = model3_alt, 
       "Model 4: Year + Region + Zoning" = model4_alt),
  stars = TRUE,
  coef_map = coef_mapping,  # Map to clean category names
  title = "Effect of Zoning Categories on Truck Stops",
  notes = paste0("Reference category: Wild Texas", 
                 "\nStandard errors in parentheses. * p < 0.1, ** p < 0.05, *** p < 0.01"),
  add_rows = tribble(
    ~term,                ~"Model 2: Year + Zoning",   ~"Model 3: Year + State + Zoning",   ~"Model 4: Year + Region + Zoning",
    "Fixed Effects:",     "",                          "",                                  "",
    "State",              "No",                        "Yes",                               "No",
    "Region",             "No",                        "No",                                "Yes"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared")
)



library(ggplot2)
library(broom)
library(dplyr)
library(tidyr)

# Function to extract coefficients and prepare for plotting
extract_coefs <- function(model, model_name) {
  # Extract coefficients
  coefs <- tidy(model)
  
  # Filter only simple_family variables
  family_coefs <- coefs %>%
    filter(grepl("simple_family", term))
  
  # Clean coefficient names
  family_coefs$category <- gsub("simple_family", "", family_coefs$term)
  
  # Add model identifier
  family_coefs$model <- model_name
  
  # Add reference category with coefficient 0
  ref_row <- data.frame(
    term = "reference",
    estimate = 0,
    std.error = 0,
    statistic = NA,
    p.value = NA,
    category = "Wild Texas",
    model = model_name
  )
  
  # Combine with reference category
  family_coefs <- bind_rows(family_coefs, ref_row)
  
  return(family_coefs)
}

# Extract coefficients from each model
model2_coefs <- extract_coefs(model2_alt, "Model 2: Year + Zoning")
model3_coefs <- extract_coefs(model3_alt, "Model 3: Year + State + Zoning")
model4_coefs <- extract_coefs(model4_alt, "Model 4: Year + Region + Zoning")

# Combine all coefficients
all_coefs <- bind_rows(model2_coefs, model3_coefs, model4_coefs)

# Create a factor for models to control their order
all_coefs$model <- factor(all_coefs$model, 
                          levels = c("Model 2: Year + Zoning", 
                                     "Model 3: Year + State + Zoning", 
                                     "Model 4: Year + Region + Zoning"))

# Calculate average coefficient for each category to determine ordering
avg_coefs <- all_coefs %>%
  group_by(category) %>%
  summarize(avg_estimate = mean(estimate, na.rm = TRUE)) %>%
  arrange(avg_estimate)  # Sort from smallest to largest

# Set category order based on average coefficients (smallest to largest)
all_coefs$category <- factor(all_coefs$category, levels = avg_coefs$category)

# Create coefficient plot
ggplot(all_coefs, aes(x = category, y = estimate, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Effect of Zoning Categories on Truck Stops",
       subtitle = "Wild Texas is the reference category (coefficient = 0)",
       x = "Zoning Category",
       y = "Coefficient Estimate",
       color = "Model",
       shape = "Model") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")