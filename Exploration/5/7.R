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

# Run models using fixest's built-in interaction capabilities
model1 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year), 
                data = df)

model2 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) | simple_family, 
                data = df)

model3 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) | Region + simple_family, 
                data = df)

model4 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) | STATE + simple_family, 
                data = df)

# Display model results
etable(model1, model2, model3, model4,
       title = "Effect of High-Fatality Crashes on Truck Stop Construction",
       digits = 3,
       keep = "year_group")

# Visualize coefficients
coef_data <- bind_rows(
  tidy(model1) %>% mutate(model = "Model 1: No Controls"),
  tidy(model2) %>% mutate(model = "Model 2: Zoning Category FE"),
  tidy(model3) %>% mutate(model = "Model 3: Region + Zoning Category FE"),
  tidy(model4) %>% mutate(model = "Model 4: State + Zoning Category FE")
) %>%
  filter(grepl("year_group", term)) %>%
  mutate(year_start = as.numeric(gsub("year_group::([0-9]+):FATALITIES_DUMMY", "\\1", term)),
         # Create clear year period labels (e.g., "2000-2001")
         year_label = paste0(year_start, "-", year_start + 1))

# Add reference year - create reference rows for each model
ref_year_value <- as.numeric(latest_year)
ref_rows <- data.frame(
  term = paste0("Reference Year"),
  estimate = 0,
  std.error = 0,
  statistic = 0,
  p.value = 1,
  model = unique(coef_data$model),
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
       subtitle = ref_annotation,
       x = "Year Period",
       y = "Coefficient (Change in Truck Stop Construction)",
       color = "Model Specification",
       shape = "Model Specification") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = guide_legend(nrow = 2))

#part 2# Run the models as before
model_explicit <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + simple_family, 
                        data = df)

model_region <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + simple_family | Region, 
                      data = df)

model_state <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + simple_family | STATE, 
                     data = df)

# Combine coefficients and clean up labels
combined_coefs <- bind_rows(
  tidy(model_explicit) %>% 
    filter(grepl("simple_family", term)) %>%
    mutate(category = gsub("simple_family::", "", term),
           model = "No Fixed Effects"),
  
  tidy(model_region) %>% 
    filter(grepl("simple_family", term)) %>%
    mutate(category = gsub("simple_family::", "", term),
           model = "Region Fixed Effects"),
  
  tidy(model_state) %>% 
    filter(grepl("simple_family", term)) %>%
    mutate(category = gsub("simple_family::", "", term),
           model = "State Fixed Effects")
)

# Clean up category names
combined_coefs$category <- gsub("simple_family", "", combined_coefs$category)

# Find reference category (the one not in the coefficients)
all_categories <- levels(df$simple_family)
included_categories <- unique(gsub("simple_family", "", combined_coefs$category))
reference_category <- all_categories[!all_categories %in% included_categories]

# Create a label for the reference category
ref_label <- ifelse(length(reference_category) > 0, 
                    paste("Reference category:", reference_category), 
                    "No reference category found")

# Set the order of models for the x-axis
model_order <- c("No Fixed Effects", "Region Fixed Effects", "State Fixed Effects")

# Set the order of categories for the colors/shapes
categories <- unique(combined_coefs$category)
if(length(reference_category) > 0) {
  categories <- c(categories, reference_category)
}

# Add reference category with zero values for each model
if(length(reference_category) > 0) {
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
}

# Create the plot with models on x-axis and categories as colored points
ggplot(combined_coefs, aes(x = model, y = estimate, color = category, shape = category)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),
                position = position_dodge(width = 0.8), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_discrete(limits = model_order) +  # Set the order of models on x-axis
  theme_minimal() +
  labs(title = "Fixed Effect Zoning Categories on Truck Stop Construction (2006-2016)",
       subtitle = ref_label,
       x = "Model Specifications",
       y = "Coefficients (Truck Stop Construction)",
       color = "Zoning Category",
       shape = "Zoning Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_color_brewer(palette = "Set2") +
  guides(color = guide_legend(nrow = 2))



# PART 3 - MODIFIED TO USE MEDIAN VALUE AS REFERENCE (without REF label)

# First, let's run models with explicit factors rather than fixed effects syntax
# This will give us all coefficients

# Get median values for each categorical variable
# For Region
region_counts <- df %>% 
  group_by(Region) %>% 
  summarize(count = n()) %>%
  arrange(count)
median_region <- region_counts$Region[ceiling(nrow(region_counts)/2)]

# For State
state_counts <- df %>% 
  group_by(STATE) %>% 
  summarize(count = n()) %>%
  arrange(count) 
median_state <- state_counts$STATE[ceiling(nrow(state_counts)/2)]

# Run models with the median categories as reference levels
df_mod <- df %>%
  mutate(
    Region = relevel(Region, ref = as.character(median_region)),
    STATE = relevel(STATE, ref = as.character(median_state))
  )

# Run the models with explicit factors instead of fixed effects syntax
model_region_alt <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + 
                            simple_family + Region, data = df_mod)

model_state_alt <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + 
                           simple_family + STATE, data = df_mod)

# Extract Region coefficients
region_coefs <- tidy(model_region_alt) %>% 
  filter(grepl("Region", term)) %>%
  mutate(
    category = gsub("Region", "", term),
    model = "Region Fixed Effects"
  )

# Extract State coefficients
state_coefs <- tidy(model_state_alt) %>% 
  filter(grepl("STATE", term)) %>%
  mutate(
    category = gsub("STATE", "", term),
    model = "State Fixed Effects"
  )

# Clean up categories
region_coefs$category <- gsub("::", "", region_coefs$category)
state_coefs$category <- gsub("::", "", state_coefs$category)

# Add reference Region with zero values
ref_region_row <- data.frame(
  term = "Reference Region",
  estimate = 0,
  std.error = 0,
  statistic = 0,
  p.value = 1,
  category = as.character(median_region),
  model = "Region Fixed Effects"
)
region_coefs <- bind_rows(region_coefs, ref_region_row)

# Add reference State with zero values
ref_state_row <- data.frame(
  term = "Reference State",
  estimate = 0,
  std.error = 0,
  statistic = 0,
  p.value = 1,
  category = as.character(median_state),
  model = "State Fixed Effects"
)
state_coefs <- bind_rows(state_coefs, ref_state_row)

# Sort regions by estimate for better visualization
region_coefs <- region_coefs %>% arrange(estimate)
region_coefs$category <- factor(region_coefs$category, levels = region_coefs$category)

# Create the Region plot
region_plot <- ggplot(region_coefs, aes(x = category, y = estimate, color = category)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),
                width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Fixed Effect of Region on Truck Stop Construction (2006-2016)",
       subtitle = paste("Reference region (median):", median_region),
       x = "Regions",
       y = "Coefficient (Truck Stop Construction)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_color_brewer(palette = "Set1")

print(region_plot)

# Sort states by coefficient estimate for better visualization
state_coefs <- state_coefs %>% arrange(estimate)
state_coefs$category <- factor(state_coefs$category, levels = state_coefs$category)

# Create the State plot
state_plot <- ggplot(state_coefs, aes(x = category, y = estimate, color = category)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Effect of State on Truck Stop Construction (2006-2016)",
       subtitle = paste("Reference state (median):", median_state),
       x = "States",
       y = "Coefficient") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        legend.position = "none") +
  coord_flip() # Use horizontal layout for better state name visibility

print(state_plot)
