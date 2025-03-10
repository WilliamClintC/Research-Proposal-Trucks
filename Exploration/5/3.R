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

# Run models with different fixed effects
# Model 1: No controls
model1 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year), 
                data = df)

# Model 2: Only simple_family controls
model2 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) | simple_family, 
                data = df)

# Model 3: simple_family and Region controls
model3 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) | simple_family + Region, 
                data = df)

# Model 4: simple_family and STATE controls
model4 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) | simple_family + STATE, 
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
  tidy(model3) %>% mutate(model = "Model 3: Zoning Category + Region FE"),
  tidy(model4) %>% mutate(model = "Model 4: Zoning Category + State FE")
) %>%
  filter(grepl("year_group", term)) %>%
  mutate(year_start = as.numeric(gsub("year_group::([0-9]+):FATALITIES_DUMMY", "\\1", term)),
         # Create clear year period labels (e.g., "2000-2001")
         year_label = paste0(year_start, "-", year_start + 1))

# Order the year labels chronologically
coef_data$year_label <- factor(coef_data$year_label, 
                               levels = unique(coef_data$year_label[order(coef_data$year_start)]))

# Plot the coefficients with clear year period labels
ggplot(coef_data, aes(x = year_label, y = estimate, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error),
                position = position_dodge(width = 0.8), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Impact of High-Fatality Crashes on Production Changes",
       x = "Year Period",
       y = "Coefficient (Effect on change_2006_2016)",
       color = "Model Specification",
       shape = "Model Specification") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = guide_legend(nrow = 2))


