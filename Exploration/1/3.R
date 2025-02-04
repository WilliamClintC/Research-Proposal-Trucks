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
process_data <- function(file_path, output_prefix, plot_label) {
  # Read the CSV file
  df <- read.csv(file_path)
  
  # Create interaction terms
  df <- dplyr::mutate(
    df,
    interaction_2002_2006 = VEHICLES_IN_ACCIDENT * indicator_2002_2006,
    interaction_1999_2001 = VEHICLES_IN_ACCIDENT * indicator_1999_2001,
    interaction_1993_1998 = VEHICLES_IN_ACCIDENT * indicator_1993_1998
  )
  
  # Fit the linear model
  model <- lm(change_2006_2016 ~ interaction_2002_2006 + interaction_1999_2001 + interaction_1993_1998, data = df)
  
  # Extract coefficients
  coefficients <- summary(model)$coefficients
  coefficients_df <- as.data.frame(coefficients)
  coefficients_df$term <- rownames(coefficients_df)
  
  # Remove the intercept
  coefficients_df <- coefficients_df %>% filter(term != "(Intercept)")
  
  # Plot the Betas
  plot <- ggplot(coefficients_df, aes(x = term, y = Estimate)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Coefficients of the Linear Model (", plot_label, ")", sep = ""), x = "Terms", y = "Estimate")
  
  # Display the plot
  print(plot)
  
  return(model)
}

# Process the first file
model1 <- process_data("C:\\Users\\clint\\Desktop\\Research-Paper-Trucks\\Exploration\\all_df.csv", "all_df", "Unconditioned")

# Process the second file
model2 <- process_data("C:\\Users\\clint\\Desktop\\Research-Paper-Trucks\\Exploration\\conditional2_merged_df.csv", "conditional2_merged_df", "Conditional on More Than 2 Truck Stops Created")

# Process the third file
model3 <- process_data("C:\\Users\\clint\\Desktop\\Research-Paper-Trucks\\Exploration\\conditional1_merged_df.csv", "conditional1_merged_df", "Conditional on More Than 1 Truck Stops Created")

# Process the new file
model4 <- process_data("C:\\Users\\clint\\Desktop\\Research-Paper-Trucks\\Exploration\\conditionalneg1_merged_df.csv", "conditionalneg1_merged_df", "Conditional on less than 1 Truck Stops Created")

# Create the combined stargazer table as HTML
stargazer_file_html <- "combined_model_summary.html"
stargazer(model1, model2, model3, model4, type = "html", title = "Linear Model Summary", 
          column.labels = c("Unconditioned", "Conditional on More Than 2 Truck Stops Created", "Conditional on More Than 1 Truck Stops Created", "Conditional on less than 1 Truck Stops Created"), 
          out = stargazer_file_html)

# Convert the HTML to an image
stargazer_file_image <- "combined_model_summary.png"
webshot(stargazer_file_html, stargazer_file_image)