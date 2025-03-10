# REGION AND STATE FIXED EFFECTS WITH YEAR FIXED EFFECTS AND MEDIAN-COEFFICIENT CATEGORY AS REFERENCE

# Load required libraries
library(dplyr)
library(ggplot2)
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

# STEP 1: Run initial models WITH YEAR FIXED EFFECTS
model_region_initial <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + 
                                simple_family + Region | year_group, data = df)

model_state_initial <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + 
                               simple_family + STATE | year_group, data = df)

# STEP 2: Extract coefficients
region_coefs_initial <- tidy(model_region_initial) %>% 
  filter(grepl("Region", term))

state_coefs_initial <- tidy(model_state_initial) %>% 
  filter(grepl("STATE", term))

# Extract clean region names from term column and handle different possible patterns
region_coefs_initial$clean_category <- gsub("^Region::|^Region", "", region_coefs_initial$term)

# Extract clean state names
state_coefs_initial$clean_category <- gsub("^STATE::|^STATE", "", state_coefs_initial$term)

# STEP 3: Add reference categories with coefficient 0
ref_region <- levels(df$Region)[1]
region_coefs_initial <- bind_rows(
  region_coefs_initial,
  data.frame(
    term = "Reference",
    estimate = 0,
    std.error = 0,
    statistic = 0,
    p.value = 1,
    clean_category = ref_region
  )
)

ref_state <- levels(df$STATE)[1]
state_coefs_initial <- bind_rows(
  state_coefs_initial,
  data.frame(
    term = "Reference",
    estimate = 0,
    std.error = 0,
    statistic = 0,
    p.value = 1,
    clean_category = ref_state
  )
)

# STEP 4: Find median coefficient
median_region_coef <- median(region_coefs_initial$estimate)
median_state_coef <- median(state_coefs_initial$estimate)

# Find the index of the closest value to the median
median_region_idx <- which.min(abs(region_coefs_initial$estimate - median_region_coef))
median_state_idx <- which.min(abs(state_coefs_initial$estimate - median_state_coef))

# Get the clean category name that's closest to median
median_region_name <- region_coefs_initial$clean_category[median_region_idx]
median_state_name <- state_coefs_initial$clean_category[median_state_idx]

# STEP 5: Check if these names match actual factor levels
cat("Region levels in data:", paste(levels(df$Region), collapse=", "), "\n")
cat("State levels in data:", paste(head(levels(df$STATE)), collapse=", "), "...\n")

cat("Median-closest region from coefficients:", median_region_name, "\n")
cat("Median-closest state from coefficients:", median_state_name, "\n")

# See if the median region/state names match actual levels
region_match <- median_region_name %in% levels(df$Region)
state_match <- median_state_name %in% levels(df$STATE)

cat("Region match found:", region_match, "\n")
cat("State match found:", state_match, "\n")

# If no match, try to find similar level names
if (!region_match) {
  # Look for similar names
  similar_regions <- agrep(median_region_name, levels(df$Region), max.distance = 0.3, value = TRUE)
  if (length(similar_regions) > 0) {
    cat("Similar region names found:", paste(similar_regions, collapse=", "), "\n")
    median_region_name <- similar_regions[1]
    region_match <- TRUE
  } else {
    cat("No similar region found, using default first level\n")
    median_region_name <- ref_region
  }
}

if (!state_match) {
  # Look for similar names
  similar_states <- agrep(median_state_name, levels(df$STATE), max.distance = 0.3, value = TRUE)
  if (length(similar_states) > 0) {
    cat("Similar state names found:", paste(similar_states, collapse=", "), "\n")
    median_state_name <- similar_states[1]
    state_match <- TRUE
  } else {
    cat("No similar state found, using default first level\n")
    median_state_name <- ref_state
  }
}

# STEP 6: Relevel the factors
cat("Setting reference region to:", median_region_name, "\n")
cat("Setting reference state to:", median_state_name, "\n")

df_mod <- df %>%
  mutate(
    Region = relevel(Region, ref = median_region_name),
    STATE = relevel(STATE, ref = median_state_name)
  )

# STEP 7: Run new models with median-closest category as reference AND YEAR FIXED EFFECTS
model_region_median <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + 
                               simple_family + Region | year_group, data = df_mod)

model_state_median <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + 
                              simple_family + STATE | year_group, data = df_mod)

# STEP 8: Extract new coefficients
region_coefs <- tidy(model_region_median) %>% 
  filter(grepl("Region", term)) %>%
  mutate(category = gsub("Region::|Region", "", term))

state_coefs <- tidy(model_state_median) %>% 
  filter(grepl("STATE", term)) %>%
  mutate(category = gsub("STATE::|STATE", "", term))

# STEP 9: Add reference categories with 0 coefficient
region_coefs <- bind_rows(region_coefs, 
                          data.frame(
                            term = "Reference",
                            estimate = 0,
                            std.error = 0,
                            statistic = 0,
                            p.value = 1,
                            category = median_region_name
                          ))

state_coefs <- bind_rows(state_coefs, 
                         data.frame(
                           term = "Reference",
                           estimate = 0,
                           std.error = 0,
                           statistic = 0,
                           p.value = 1,
                           category = median_state_name
                         ))

# STEP 10: Sort coefficients for plotting
region_coefs <- region_coefs %>% arrange(estimate)
region_coefs$category <- factor(region_coefs$category, levels = region_coefs$category)

state_coefs <- state_coefs %>% arrange(estimate)
state_coefs$category <- factor(state_coefs$category, levels = state_coefs$category)

# STEP 11: Create plots
region_plot <- ggplot(region_coefs, aes(x = category, y = estimate, color = category)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),
                width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Fixed Effect of Region on Truck Stop Construction (2006-2016)",
       subtitle = paste("Reference region (median effect):", median_region_name, 
                        "- With Year Fixed Effects"),
       x = "Regions",
       y = "Coefficient (Truck Stop Construction)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_color_brewer(palette = "Set1")

state_plot <- ggplot(state_coefs, aes(x = category, y = estimate, color = category)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Effect of State on Truck Stop Construction (2006-2016)",
       subtitle = paste("Reference state (median effect):", median_state_name,
                        "- With Year Fixed Effects"),
       x = "States",
       y = "Coefficient") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        legend.position = "none") +
  coord_flip()

# Print and save plots
print(region_plot)
ggsave("region_fixed_effects_with_year_fe.png", plot = region_plot, width = 10, height = 7)

print(state_plot)
ggsave("state_fixed_effects_with_year_fe.png", plot = state_plot, width = 12, height = 10)