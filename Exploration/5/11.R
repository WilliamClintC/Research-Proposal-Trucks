# Load required libraries
library(dplyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(tibble)
library(broom)

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

# Set "Traditional" as the reference category
df$simple_family <- relevel(df$simple_family, ref = "Traditional")

# Run models with year fixed effects and clustered standard errors
model_explicit <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + simple_family | year_group, 
                        data = df,
                        cluster = "Region")  # Cluster at Region level

model_region <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + simple_family | year_group + Region, 
                      data = df,
                      cluster = "Region")  # Cluster at Region level

model_state <- feols(change_2006_2016 ~ i(year_group, FATALITIES_DUMMY, ref = latest_year) + simple_family | year_group + STATE, 
                     data = df,
                     cluster = "Region")  # Cluster at Region level

# Check the actual coefficient names
coef_names <- names(coef(model_explicit))
cat("First few coefficient names:\n")
print(head(coef_names))

# Create a list of models with descriptive names
models_list <- list(
  "Year FE Only" = model_explicit,
  "Year + Region FE" = model_region, 
  "Year + State FE" = model_state
)

# Find all simple_family coefficients in the first model
simple_family_coeffs <- coef_names[grep("simple_family", coef_names)]

# Create a dynamic coef_map based on the actual coefficient names
coef_map <- setNames(
  gsub("simple_family", "", simple_family_coeffs), 
  simple_family_coeffs
)

# Create indicator rows for which controls are included
rows <- tribble(
  ~term,              ~"Year FE Only", ~"Year + Region FE", ~"Year + State FE",
  "Year FE",             "Yes",         "Yes",                "Yes",
  "Region FE",           "No",          "Yes",                "No",
  "State FE",            "No",          "No",                 "Yes"
)

# Create summary table showing ONLY the simple_family (zoning) coefficients
modelsummary(models_list,
             stars = TRUE,
             title = "Effect of Zoning Categories on Truck Stop Construction (2006-2016)",
             coef_map = coef_map,
             gof_map = c("nobs", "r2", "adj.r2"),
             add_rows = rows,
             note = "Reference category: Traditional. All models include year fixed effects.")