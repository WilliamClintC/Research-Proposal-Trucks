# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stargazer)
library(rstudioapi)
library(webshot)
library(htmltools)

# Change working directory to the current script's directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Define a function to process the data and generate outputs
process_data <- function(file_path, plot_label, include_fixed_effects = TRUE, simple_family_level = NULL) {
  # Read the CSV file
  df <- read.csv(file_path)
  
  # Filter the data if a simple_family_level is provided
  if (!is.null(simple_family_level)) {
    df <- df %>% filter(simple_family == simple_family_level)
  }
  
  # Create interaction terms
  df <- dplyr::mutate(
    df,
    interaction_2002_2006 = FATALITIES * indicator_2002_2006,
    interaction_1999_2001 = FATALITIES * indicator_1999_2001,
    interaction_1993_1998 = FATALITIES * indicator_1993_1998
  )
  
  # Fit the linear model
  if (include_fixed_effects) {
    model <- lm(change_2006_2016 ~ interaction_2002_2006 + interaction_1999_2001 + interaction_1993_1998 + factor(simple_family), data = df)
  } else {
    model <- lm(change_2006_2016 ~ interaction_2002_2006 + interaction_1999_2001 + interaction_1993_1998, data = df)
  }
  
  # Extract coefficients
  coefficients <- summary(model)$coefficients
  coefficients_df <- as.data.frame(coefficients)
  coefficients_df$term <- rownames(coefficients_df)
  
  # Filter to include only interaction terms
  interaction_terms <- c("interaction_2002_2006", "interaction_1999_2001", "interaction_1993_1998")
  coefficients_df <- coefficients_df %>% filter(term %in% interaction_terms)
  
  # Plot the Betas
  plot <- ggplot(coefficients_df, aes(x = term, y = Estimate)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Coefficients of the Linear Model (", plot_label, ")", sep = ""), x = "Terms", y = "Estimate")
  
  # Display the plot
  print(plot)
  
  return(model)
}

# Process the specified file for the overall model with fixed effects
model_with_fixed_effects <- process_data("C:\\Users\\clint\\Desktop\\Research-Paper-Trucks\\Exploration\\3\\df_exported.csv", "Interactions with Fixed Effects")

# Get the levels of the simple_family factor
df <- read.csv("C:\\Users\\clint\\Desktop\\Research-Paper-Trucks\\Exploration\\3\\df_exported.csv")
simple_family_levels <- unique(df$simple_family)

# Initialize a list to store models
models <- list(model_with_fixed_effects)

# Loop through each level of simple_family and run the model without fixed effects
for (level in simple_family_levels) {
  plot_label <- paste("Interactions Conditional on", level)
  model <- process_data("C:\\Users\\clint\\Desktop\\Research-Paper-Trucks\\Exploration\\3\\df_exported.csv", plot_label, include_fixed_effects = FALSE, simple_family_level = level)
  models[[length(models) + 1]] <- model
}

# Create the stargazer table as HTML for all models
stargazer_file_html <- "combined_model_summary.html"
stargazer(models, type = "html", title = "Linear Model Summary", 
          column.labels = c("Interactions with Fixed Effects", paste("Conditional on", simple_family_levels)), 
          out = stargazer_file_html)

# Convert the HTML to an image for the combined model summary
stargazer_file_image <- "combined_model_summary.png"
webshot(stargazer_file_html, stargazer_file_image)