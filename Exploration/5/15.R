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
  mutate(FATALITIES_DUMMY = ifelse(FATALITIES >= 1, 1, 0)) %>%
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

#------------------------------------------------
# PART 5: Conditional Models by Zoning Category
#------------------------------------------------

# First, check the distribution of simple_family categories
table_results <- table(df$simple_family)
print(table_results)

# Create a list to store models for each category
models_by_category <- list()

# Get all unique categories
categories <- levels(df$simple_family)
print(paste("Working with categories:", paste(categories, collapse=", ")))

# For each category, run models on the subset of data
for(cat in categories) {
  # Subset the data
  df_subset <- df %>% filter(simple_family == cat)
  
  cat("\nModels for category:", cat, "- Sample size:", nrow(df_subset), "\n")
  
  # Skip if sample size is too small
  if(nrow(df_subset) < 5) {
    cat("Sample size too small, skipping\n")
    next
  }
  
  # Model 1: Year fixed effects only
  model1 <- tryCatch({
    feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) | year_group, 
          data = df_subset)
  }, error = function(e) {
    cat("Error in model 1:", e$message, "\n")
    return(NULL)
  })
  
  # Model 2: Year + Region fixed effects
  model2 <- tryCatch({
    feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) | year_group + Region, 
          data = df_subset)
  }, error = function(e) {
    cat("Error in model 2:", e$message, "\n")
    return(NULL)
  })
  
  # Model 3: Year + State fixed effects
  model3 <- tryCatch({
    feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) | year_group + STATE, 
          data = df_subset)
  }, error = function(e) {
    cat("Error in model 3:", e$message, "\n")
    return(NULL)
  })
  
  # Store models if they were successfully created
  if(!is.null(model1) || !is.null(model2) || !is.null(model3)) {
    models_by_category[[cat]] <- list(
      "Year FE" = model1,
      "Year + Region FE" = model2,
      "Year + State FE" = model3
    )
  }
}

# Display results for each category
for(cat in names(models_by_category)) {
  cat("\n\n*** Models for category:", cat, "***\n")
  
  # Get valid models for this category
  valid_models <- models_by_category[[cat]][!sapply(models_by_category[[cat]], is.null)]
  
  if(length(valid_models) > 0) {
    # Display model results with etable
    do.call(etable, c(valid_models, 
                      list(title = paste("Effect of High-Fatality Crashes -", cat, "Zoning"),
                           digits = 3,
                           keep = "%FATALITIES_DUMMY")))
  } else {
    cat("No valid models for this category\n")
  }
}

# Create visualizations for each category
for(cat in names(models_by_category)) {
  # Skip categories with no valid models
  if(all(sapply(models_by_category[[cat]], is.null))) next
  
  # Prepare data frame to collect coefficients
  coef_data <- data.frame()
  
  # For each model type, extract coefficients
  model_types <- c("Year FE", "Year + Region FE", "Year + State FE")
  for(model_type in model_types) {
    model <- models_by_category[[cat]][[model_type]]
    if(!is.null(model)) {
      # Extract and format coefficients
      model_coefs <- tidy(model) %>% 
        filter(grepl("FATALITIES_DUMMY", term)) %>%
        mutate(
          model = model_type,
          year_start = as.numeric(gsub("year_group::([0-9]+):FATALITIES_DUMMY", "\\1", term)),
          year_label = paste0(year_start, "-", year_start + 1)
        )
      
      # Add reference year
      ref_row <- data.frame(
        term = "Reference Year",
        estimate = 0,
        std.error = 0,
        statistic = 0,
        p.value = 1,
        model = model_type,
        year_start = as.numeric(latest_year),
        year_label = paste0(latest_year, "-", as.numeric(latest_year) + 1)
      )
      
      # Combine
      coef_data <- bind_rows(coef_data, model_coefs, ref_row)
    }
  }
  
  # Skip if no coefficients were collected
  if(nrow(coef_data) == 0) next
  
  # Order the year labels chronologically
  coef_data$year_label <- factor(coef_data$year_label, 
                                 levels = unique(coef_data$year_label[order(coef_data$year_start)]))
  
  # Add reference label annotation
  ref_annotation <- paste("Reference year:", paste0(latest_year, "-", as.numeric(latest_year) + 1))
  
  # Plot the coefficients
  plot <- ggplot(coef_data, aes(x = year_label, y = estimate, color = model, shape = model)) +
    geom_point(position = position_dodge(width = 0.8), size = 3) +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error),
                  position = position_dodge(width = 0.8), width = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(title = paste("Effect of High-Fatality Crashes -", cat, "Zoning Only"),
         subtitle = paste(ref_annotation),
         x = "Year Period",
         y = "Coefficient (Change in Truck Stop Construction)",
         color = "Model Specification",
         shape = "Model Specification") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(color = guide_legend(nrow = 2))
  
  print(plot)
}

# Create a combined visualization comparing all categories using the simplest model
combined_coefs <- data.frame()

for(cat in names(models_by_category)) {
  # Get the Year FE model (simplest)
  model <- models_by_category[[cat]][["Year FE"]]
  if(!is.null(model)) {
    # Extract coefficients
    cat_coefs <- tidy(model) %>% 
      filter(grepl("FATALITIES_DUMMY", term)) %>%
      mutate(
        category = cat,
        year_start = as.numeric(gsub("year_group::([0-9]+):FATALITIES_DUMMY", "\\1", term)),
        year_label = paste0(year_start, "-", year_start + 1)
      )
    
    # Add reference year
    ref_row <- data.frame(
      term = "Reference Year",
      estimate = 0,
      std.error = 0,
      statistic = 0,
      p.value = 1,
      category = cat,
      year_start = as.numeric(latest_year),
      year_label = paste0(latest_year, "-", as.numeric(latest_year) + 1)
    )
    
    # Combine
    combined_coefs <- bind_rows(combined_coefs, cat_coefs, ref_row)
  }
}

# Create the combined comparison plot if we have data
if(nrow(combined_coefs) > 0) {
  # Order the year labels chronologically
  combined_coefs$year_label <- factor(combined_coefs$year_label, 
                                      levels = unique(combined_coefs$year_label[order(combined_coefs$year_start)]))
  
  # Create the comparison plot
  comparison_plot <- ggplot(combined_coefs, aes(x = year_label, y = estimate, color = category, shape = category)) +
    geom_point(position = position_dodge(width = 0.8), size = 3) +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                      ymax = estimate + 1.96*std.error),
                  position = position_dodge(width = 0.8), width = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(title = "Effect of High-Fatality Crashes by Zoning Category",
         subtitle = paste("Reference year:", paste0(latest_year, "-", as.numeric(latest_year) + 1), 
                          "- Models run separately for each zoning type"),
         x = "Year Period",
         y = "Coefficient (Change in Truck Stop Construction)",
         color = "Zoning Category",
         shape = "Zoning Category") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(color = guide_legend(nrow = 2))
  
  print(comparison_plot)
}

# Create summary table with all models by category
models_for_summary <- list()
for(cat in names(models_by_category)) {
  # Add each valid model to the list with a descriptive name
  for(model_type in names(models_by_category[[cat]])) {
    model <- models_by_category[[cat]][[model_type]]
    if(!is.null(model)) {
      models_for_summary[[paste(cat, "-", model_type)]] <- model
    }
  }
}

# Create comprehensive model summary if we have models
if(length(models_for_summary) > 0) {
  modelsummary(models_for_summary,
               stars = TRUE,
               title = "Effect of High-Fatality Crashes by Zoning Category",
               coef_map = c("year_group::1996:FATALITIES_DUMMY" = "1996-1997 × High Fatality",
                            "year_group::1998:FATALITIES_DUMMY" = "1998-1999 × High Fatality", 
                            "year_group::2000:FATALITIES_DUMMY" = "2000-2001 × High Fatality",
                            "year_group::2002:FATALITIES_DUMMY" = "2002-2003 × High Fatality",
                            "year_group::2004:FATALITIES_DUMMY" = "2004-2005 × High Fatality"),
               gof_map = c("nobs", "r2", "adj.r2", "rmse"),
               note = paste("Reference period:", latest_year, "-", as.numeric(latest_year)+1,
                            ". Models run separately for each zoning category."),
               output = "zoning_category_models_comparison.html")
}


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

# Check for NAs in standard errors
print(paste("Number of NAs in adjusted_se:", sum(is.na(all_fe_adjusted$adjusted_se))))

# Replace any NA standard errors with a small value to avoid plotting issues
all_fe_adjusted <- all_fe_adjusted %>%
  mutate(
    adjusted_se = ifelse(is.na(adjusted_se), 0.01, adjusted_se),
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

# Print the range of CIs to check they're meaningful
print("Range of confidence interval values:")
print(summary(all_fe_adjusted$lower_ci))
print(summary(all_fe_adjusted$upper_ci))

# Create plot with very explicit error bars
p <- ggplot(all_fe_adjusted, aes(x = category, y = adjusted_estimate)) +
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

# Print the plot
print(p)