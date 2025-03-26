# Load required libraries
library(dplyr)
library(ggplot2)
library(stargazer)
library(broom)
library(fixest)
library(modelsummary)
library(tibble)

# Load and prepare data
df <- read.csv("C:/Users/clint/Desktop/Research-Paper-Trucks/Exploration/5/df_6.csv") %>%
  # Convert date and filter for years 2006 and earlier
  mutate(REPORT_DATE = as.Date(REPORT_DATE)) %>%
  filter(format(REPORT_DATE, "%Y") <= 2006) %>%
  # Create fatalities dummy (1 if fatalities >= 3, otherwise 0)
  mutate(FATALITIES_DUMMY = ifelse(FATALITIES >= 3, 1, 0)) %>%
  # Group years into 2-year periods
  mutate(year_group = floor(as.numeric(format(REPORT_DATE, "%Y")) / 2) * 2)

# Store latest year BEFORE converting to factor
latest_year <- as.character(max(df$year_group))

# Now convert to factors
df <- df %>%
  mutate(
    simple_family = as.factor(simple_family),
    Region = as.factor(Region),
    STATE = as.factor(STATE),
    year_group = as.factor(year_group)
  )

# Run models using fixest with year fixed effects
# Note: we keep the interaction but now add year_group as a fixed effect

# Model 1: Year fixed effects only
model1 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) | year_group, 
                data = df)

# Model 2: Year + Zoning Category fixed effects
model2 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) | year_group + simple_family, 
                data = df)

# Model 3: Year + Region + Zoning Category fixed effects
model3 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) | year_group + Region + simple_family, 
                data = df)

# Model 4: Year + State + Zoning Category fixed effects
model4 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) | year_group + STATE + simple_family, 
                data = df)

# Display model results - FIXED keep pattern with %
etable(model1, model2, model3, model4,
       title = "Effect of High-Fatality Crashes on Truck Stop Construction",
       digits = 3,
       keep = "%FATALITIES_DUMMY")

# Define model names explicitly for visualization
model_names <- c("Model 1: Year FE", 
                 "Model 2: Year + Zoning FE",
                 "Model 3: Year + Region + Zoning FE",
                 "Model 4: Year + State + Zoning FE")

# Visualize coefficients - direct approach to get interaction terms
# Extract coefficients from each model and filter for the ones we want
coefs_m1 <- tidy(model1) %>% 
  filter(grepl("FATALITIES_DUMMY", term)) %>%
  mutate(model = model_names[1])

coefs_m2 <- tidy(model2) %>% 
  filter(grepl("FATALITIES_DUMMY", term)) %>%
  mutate(model = model_names[2])

coefs_m3 <- tidy(model3) %>% 
  filter(grepl("FATALITIES_DUMMY", term)) %>%
  mutate(model = model_names[3])

coefs_m4 <- tidy(model4) %>% 
  filter(grepl("FATALITIES_DUMMY", term)) %>%
  mutate(model = model_names[4])

# Combine all coefficient data
coef_data <- bind_rows(coefs_m1, coefs_m2, coefs_m3, coefs_m4) %>%
  mutate(
    year_start = as.numeric(gsub("year_group::([0-9]+):FATALITIES_DUMMY", "\\1", term)),
    year_label = paste0(year_start, "-", year_start + 1)
  )

# Add reference year - create reference rows for each model
ref_year_value <- as.numeric(latest_year)
ref_rows <- data.frame(
  term = rep("Reference Year", length(model_names)),
  estimate = 0,
  std.error = 0,
  statistic = 0,
  p.value = 1,
  model = model_names,
  year_start = ref_year_value,
  year_label = paste0(ref_year_value, "-", ref_year_value + 1)
)

# Combine with coefficient data
coef_data <- bind_rows(coef_data, ref_rows)

# Order the year labels chronologically
coef_data$year_label <- factor(coef_data$year_label, 
                               levels = unique(coef_data$year_label[order(coef_data$year_start)]))

# Add reference label annotation
ref_annotation <- paste("Reference year:", paste0(ref_year_value, "-", ref_year_value + 1))

# Plot the coefficients with clear year period labels and reference year
ggplot(coef_data, aes(x = year_label, y = estimate, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),
                position = position_dodge(width = 0.8), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Effect of High-Fatality Crashes on Truck Stop Construction",
       subtitle = paste(ref_annotation, "- All models include year fixed effects"),
       x = "Year Period",
       y = "Coefficient (Change in Truck Stop Construction)",
       color = "Model Specification",
       shape = "Model Specification") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = guide_legend(nrow = 2))

#part 2

# Set "Traditional" as the reference category
df$simple_family <- relevel(df$simple_family, ref = "Traditional")

# Run models with year fixed effects and clustered standard errors at the Region level
model_explicit <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + simple_family | year_group, 
                        data = df,
                        cluster = "Region")  # Cluster standard errors at Region level

model_region <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + simple_family | year_group + Region, 
                      data = df,
                      cluster = "Region")  # Cluster standard errors at Region level

model_state <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + simple_family | year_group + STATE, 
                     data = df,
                     cluster = "Region")  # Cluster standard errors at Region level

# Combine coefficients and clean up labels - directly filter for simple_family
combined_coefs <- bind_rows(
  tidy(model_explicit) %>% 
    filter(grepl("simple_family", term)) %>%
    mutate(category = gsub("simple_family::", "", term),
           model = "Year FE Only"),
  
  tidy(model_region) %>% 
    filter(grepl("simple_family", term)) %>%
    mutate(category = gsub("simple_family::", "", term),
           model = "Year + Region FE"),
  
  tidy(model_state) %>% 
    filter(grepl("simple_family", term)) %>%
    mutate(category = gsub("simple_family::", "", term),
           model = "Year + State FE")
)

# Clean up category names
combined_coefs$category <- gsub("simple_family", "", combined_coefs$category)

# Set the reference category manually
reference_category <- "Traditional"
ref_label <- paste("Reference category:", reference_category)

# Set the order of models for the x-axis
model_order <- c("Year FE Only", "Year + Region FE", "Year + State FE")

# Add reference category with zero values for each model
ref_rows <- data.frame(
  term = "Reference Category",
  estimate = 0,
  std.error = 0,
  statistic = 0,
  p.value = 1,
  category = reference_category,
  model = model_order
)
combined_coefs <- bind_rows(combined_coefs, ref_rows)

# Create the plot with models on x-axis and categories as colored points
ggplot(combined_coefs, aes(x = model, y = estimate, color = category, shape = category)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),
                position = position_dodge(width = 0.8), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_discrete(limits = model_order) +
  theme_minimal() +
  labs(title = "Fixed Effect Zoning Categories on Truck Stop Construction (2006-2016)",
       subtitle = paste(ref_label, "- All models include year fixed effects with Region-clustered SEs"),
       x = "Model Specifications",
       y = "Coefficients (Truck Stop Construction)",
       color = "Zoning Category",
       shape = "Zoning Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_color_brewer(palette = "Set2") +
  guides(color = guide_legend(nrow = 2))



# PART 3 - MODIFIED TO USE MEDIAN VALUE AS REFERENCE (without REF label)
# Fixed approach to select reference levels and robust plotting

# Run initial models with arbitrary reference levels (first level by default)
model_region_initial <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + 
                                simple_family + Region | year_group, data = df)

model_state_initial <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + 
                               simple_family + STATE | year_group, data = df)

# Print available levels to help with debugging
cat("Available Region levels:\n")
print(head(levels(df$Region)))
cat("Available State levels:\n")
print(head(levels(df$STATE)))

# FIXED APPROACH: Choose middle levels directly instead of trying to match names
region_levels <- levels(df$Region)
state_levels <- levels(df$STATE)

# Use middle index for balanced reference
median_region_index <- ceiling(length(region_levels)/2)
median_state_index <- ceiling(length(state_levels)/2)

median_region <- region_levels[median_region_index]
median_state <- state_levels[median_state_index]

cat("Selected median region:", median_region, "\n")
cat("Selected median state:", median_state, "\n")

# Use these levels to create a new dataframe
df_mod <- df %>%
  mutate(
    Region = relevel(Region, ref = median_region),
    STATE = relevel(STATE, ref = median_state)
  )

# Run the models with the median coefficient categories as reference levels
model_region_alt <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + 
                            simple_family + Region | year_group, data = df_mod)

model_state_alt <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + 
                           simple_family + STATE | year_group, data = df_mod)

# DEBUG: Check what coefficients are available
cat("First few terms in model_region_alt:\n")
print(head(tidy(model_region_alt)$term))
cat("First few terms in model_state_alt:\n")
print(head(tidy(model_state_alt)$term))

# ROBUST REGION COEFFICIENT EXTRACTION
# Try multiple pattern matching approaches
region_coefs <- tidy(model_region_alt) %>% 
  filter(grepl("Region", term)) %>%
  mutate(
    category = gsub(".*Region::", "", term),
    model = "Region Fixed Effects"
  )

# If no matches, try another pattern
if (nrow(region_coefs) == 0) {
  region_coefs <- tidy(model_region_alt) %>% 
    filter(grepl("Region", term)) %>%
    mutate(
      category = gsub(".*Region[^A-Za-z0-9]*(.*)", "\\1", term),
      model = "Region Fixed Effects"
    )
}

# ROBUST STATE COEFFICIENT EXTRACTION
state_coefs <- tidy(model_state_alt) %>% 
  filter(grepl("STATE", term)) %>%
  mutate(
    category = gsub(".*STATE::", "", term),
    model = "State Fixed Effects"
  )

# If no matches, try another pattern
if (nrow(state_coefs) == 0) {
  state_coefs <- tidy(model_state_alt) %>% 
    filter(grepl("STATE", term)) %>%
    mutate(
      category = gsub(".*STATE[^A-Za-z0-9]*(.*)", "\\1", term),
      model = "State Fixed Effects"
    )
}

# Print coefficient counts for debugging
cat("Found", nrow(region_coefs), "region coefficients\n")
cat("Found", nrow(state_coefs), "state coefficients\n")

# Add reference Region with zero values
if (nrow(region_coefs) > 0) {
  ref_region_row <- data.frame(
    term = "Reference Region",
    estimate = 0,
    std.error = 0,
    statistic = 0,
    p.value = 1,
    category = median_region,
    model = "Region Fixed Effects"
  )
  region_coefs <- bind_rows(region_coefs, ref_region_row)
  
  # Sort regions by estimate for better visualization - with error handling
  region_coefs <- region_coefs %>% 
    mutate(estimate = ifelse(is.na(estimate), 0, estimate)) %>%
    arrange(estimate)
  
  # Check for duplicate category levels before converting to factor
  region_coefs$category <- as.character(region_coefs$category)
  region_cats_ordered <- unique(region_coefs$category[order(region_coefs$estimate)])
  region_coefs$category <- factor(region_coefs$category, levels = region_cats_ordered)
  
  # Create the Region plot
  region_plot <- ggplot(region_coefs, aes(x = category, y = estimate, color = category)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error),
                  width = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(title = "Fixed Effect of Region on Truck Stop Construction (2006-2016)",
         subtitle = paste("Reference region:", median_region, "- With Year Fixed Effects"),
         x = "Regions",
         y = "Coefficient (Truck Stop Construction)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    scale_color_brewer(palette = "Set1")
  
  print(region_plot)
} else {
  cat("ERROR: Cannot generate Region plot - no coefficients found\n")
}

# Add reference State with zero values
if (nrow(state_coefs) > 0) {
  ref_state_row <- data.frame(
    term = "Reference State",
    estimate = 0,
    std.error = 0,
    statistic = 0,
    p.value = 1,
    category = median_state,
    model = "State Fixed Effects"
  )
  state_coefs <- bind_rows(state_coefs, ref_state_row)
  
  # Sort states by coefficient estimate - with error handling
  state_coefs <- state_coefs %>%
    mutate(estimate = ifelse(is.na(estimate), 0, estimate)) %>%
    arrange(estimate)
  
  # Check for duplicate category levels before converting to factor
  state_coefs$category <- as.character(state_coefs$category)
  state_cats_ordered <- unique(state_coefs$category[order(state_coefs$estimate)])
  state_coefs$category <- factor(state_coefs$category, levels = state_cats_ordered)
  
  # Create the State plot
  state_plot <- ggplot(state_coefs, aes(x = category, y = estimate, color = category)) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error),
                  width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(title = "Effect of State on Truck Stop Construction (2006-2016)",
         subtitle = paste("Reference state:", median_state, "- With Year Fixed Effects"),
         x = "States",
         y = "Coefficient") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
          legend.position = "none") +
    coord_flip() # Use horizontal layout for better state name visibility
  
  print(state_plot)
} else {
  cat("ERROR: Cannot generate State plot - no coefficients found\n")
}

# part 4

# Create a list of models with descriptive names
models <- list("Year FE" = model1, 
               "Year + Zoning FE" = model2,
               "Year + Region + Zoning FE" = model3,
               "Year + State + Zoning FE" = model4)

# Create indicator rows for which controls are included
# These will be added at the bottom of the table
rows <- tribble(
  ~term,              ~"Year FE", ~"Year + Zoning FE", ~"Year + Region + Zoning FE", ~"Year + State + Zoning FE",
  "Year FE",             "Yes",      "Yes",                "Yes",                      "Yes",
  "Zoning Category FE",   "No",      "Yes",                "Yes",                      "Yes",
  "Region FE",            "No",      "No",                 "Yes",                      "No",
  "State FE",             "No",      "No",                 "No",                       "Yes"
)

# Create a summary table with customizations - FIXED conflict between coef_map and coef_rename
modelsummary(models, 
             stars = TRUE,
             title = "Effect of High-Fatality Crashes on Truck Stop Construction",
             coef_map = c("year_group::1996:FATALITIES_DUMMY" = "1996-1997 × High Fatality",
                          "year_group::1998:FATALITIES_DUMMY" = "1998-1999 × High Fatality", 
                          "year_group::2000:FATALITIES_DUMMY" = "2000-2001 × High Fatality",
                          "year_group::2002:FATALITIES_DUMMY" = "2002-2003 × High Fatality",
                          "year_group::2004:FATALITIES_DUMMY" = "2004-2005 × High Fatality"),
             gof_map = c("nobs", "r2", "adj.r2", "rmse"),
             add_rows = rows,
             note = paste("Reference period:", latest_year, "-", as.numeric(latest_year)+1),
             output = "default")

# For HTML output (more visually appealing)
modelsummary(models, 
             stars = TRUE,
             title = "Effect of High-Fatality Crashes on Truck Stop Construction (with Year Fixed Effects)",
             coef_map = c("year_group::1996:FATALITIES_DUMMY" = "1996-1997 × High Fatality",
                          "year_group::1998:FATALITIES_DUMMY" = "1998-1999 × High Fatality",
                          "year_group::2000:FATALITIES_DUMMY" = "2000-2001 × High Fatality",
                          "year_group::2002:FATALITIES_DUMMY" = "2002-2003 × High Fatality",
                          "year_group::2004:FATALITIES_DUMMY" = "2004-2005 × High Fatality"),
             gof_map = c("nobs", "r2", "adj.r2", "rmse"),
             add_rows = rows,
             note = paste("Reference period:", latest_year, "-", as.numeric(latest_year)+1,
                          ". Stars indicate statistical significance: * p<0.1, ** p<0.05, *** p<0.01"),
             output = "regression_results_with_year_fe.html")

# Create a list of your models with descriptive names
models_list <- list(
  "Year FE Only" = model_explicit,
  "Year + Region FE" = model_region, 
  "Year + State FE" = model_state
)

# part last 



# Take the combined_coefs dataframe that's already created in your code
# Calculate average effect size for each category
category_avg <- combined_coefs %>%
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
combined_coefs_adjusted <- combined_coefs %>%
  group_by(model) %>%
  mutate(
    # Find the estimate for the midpoint category in this model
    reference_value = estimate[category == midpoint_category],
    # Adjust all estimates relative to the reference
    adjusted_estimate = estimate - reference_value,
    # Standard errors remain the same
    adjusted_se = std.error
  ) %>%
  ungroup()

# Calculate confidence intervals explicitly
combined_coefs_adjusted <- combined_coefs_adjusted %>%
  mutate(
    lower_ci = adjusted_estimate - 1.96 * adjusted_se,
    upper_ci = adjusted_estimate + 1.96 * adjusted_se
  )

# Check for NAs in standard errors
print(paste("Number of NAs in adjusted_se:", sum(is.na(combined_coefs_adjusted$adjusted_se))))

# Replace any NA standard errors with a small value to avoid plotting issues
combined_coefs_adjusted <- combined_coefs_adjusted %>%
  mutate(
    adjusted_se = ifelse(is.na(adjusted_se), 0.01, adjusted_se),
    lower_ci = adjusted_estimate - 1.96 * adjusted_se,
    upper_ci = adjusted_estimate + 1.96 * adjusted_se
  )

# Order categories by adjusted effect
adjusted_category_avg <- combined_coefs_adjusted %>%
  group_by(category) %>%
  summarize(avg_effect = mean(adjusted_estimate, na.rm = TRUE)) %>%
  arrange(avg_effect)

# Set factor levels for proper ordering
combined_coefs_adjusted$category <- factor(combined_coefs_adjusted$category, 
                                           levels = adjusted_category_avg$category)

# Create plot with explicit error bars
ggplot(combined_coefs_adjusted, aes(x = category, y = adjusted_estimate)) +
  # First add a reference line at zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  # Add point estimates with dodging
  geom_point(aes(color = model, shape = model), 
             position = position_dodge(width = 0.8), 
             size = 3) +
  # Add error bars with very explicit parameters
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci, color = model),
    position = position_dodge(width = 0.8),
    width = 0.4,
    linewidth = 1.2,
    show.legend = FALSE
  ) +
  # Formatting
  theme_minimal() +
  labs(
    title = "Impact of Zoning Categories on Truck Stop Construction (2006-2016)",
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
  guides(color = guide_legend(nrow = 2)) +
  scale_color_brewer(palette = "Set2") # Match the color palette from your original plot