# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stargazer)
install.packages("fixest")
library(fixest)

# Load the data from the CSV file
df <- read.csv("C:/Users/clint/Desktop/Research-Paper-Trucks/Exploration/5/df_6.csv")

# Convert REPORT_DATE to Date type
df <- df %>% 
  mutate(REPORT_DATE = as.Date(REPORT_DATE))

# Filter data for years 2006 and earlier
df <- df %>% 
  filter(as.numeric(format(REPORT_DATE, "%Y")) <= 2006)

# Create a dummy variable for FATALITIES (1 if FATALITIES >= 3, else 0)
df <- df %>% 
  mutate(FATALITIES_DUMMY = ifelse(FATALITIES >= 3, 1, 0))

# Create a time variable (2-year groups)
df <- df %>% 
  mutate(year_group = floor(as.numeric(format(REPORT_DATE, "%Y")) / 2) * 2)

# Ensure categorical variables are factors
df <- df %>% 
  mutate(simple_family = as.factor(simple_family),
         Region = as.factor(Region),
         STATE = as.factor(STATE))

# Set the baseline as the most recent year group
baseline_year <- max(df$year_group, na.rm = TRUE)

# Run event study models using fixest's i() function:
# The i() function automatically creates dummies for each year_group interacted with FATALITIES_DUMMY.
# The ref = baseline_year argument sets the most recent year group as the baseline.

# Model 1: No fixed effects (i.e. the original event study model)
model1 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = baseline_year), data = df)

# Model 2: Controlling for zoning categories (simple_family)
model2 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = baseline_year) | simple_family, data = df)

# Model 3: Controlling for simple_family and Region
model3 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = baseline_year) | simple_family + Region, data = df)

# Model 4: Controlling for simple_family and STATE
model4 <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = baseline_year) | simple_family + STATE, data = df)

# Display summaries of the models
summary(model1)
summary(model2)
summary(model3)
summary(model4)

# Optionally, display the model summaries using stargazer
stargazer(model1, model2, model3, model4, type = "html", title = "Event Study Model Results", 
          column.labels = c("Original Model", "Model with Zoning Categories", 
                            "Model with Zoning Categories and Region", "Model with Zoning Categories and State"), 
          out = "C:/Users/clint/Desktop/Research-Paper-Trucks/Exploration/5/model_summary_combined.html")

# Plot the event study coefficients for model1 using fixest's built-in iplot function.
iplot(model1, main = paste("Event Study Coefficients (Baseline:", baseline_year, ")"))
