# Setup script for GenAI experiment analysis
# This script installs and loads all required packages and sets up the environment

# Function to check if a package is installed, install if not
check_and_install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, repos = "https://cloud.r-project.org")
    library(package, character.only = TRUE)
  }
}

# Install and load pacman if not already installed
if (!require("pacman")) {
  install.packages("pacman", repos = "https://cloud.r-project.org")
}

# Load pacman
library(pacman)

# List of required packages
required_packages <- c(
  # Data manipulation and analysis
  "dplyr",
  "tidyr",
  "readr",
  "readxl",
  "purrr",
  "forcats",
  "stringr",
  
  # Visualization
  "ggplot2",
  "RColorBrewer",
  
  # Statistical analysis
  "broom",
  "car",
  "stargazer",
  "boot",
  
  # Text analysis
  "tm",
  "wordcloud",
  
  # Correlation analysis
  "corrplot"
)

# Install and load all required packages
p_load(required_packages, character.only = TRUE)

# Create necessary directories if they don't exist
dirs_to_create <- c(
  "data/raw",
  "data/processed",
  "reports/figures",
  "reports/models",
  "reports/figures/initial_balance",
  "reports/figures/overlapping_histograms",
  "reports/figures/location_histograms",
  "reports/figures/order_effect_histograms",
  "reports/figures/main_test",
  "reports/figures/change_in_interest",
  "reports/figures/future_suggestion",
  "reports/figures/strength_weakness",
  "reports/models/main_test",
  "reports/models/change_in_interest",
  "reports/models/future_suggestion",
  "reports/models/strength_weakness"
)

# Create directories
for (dir in dirs_to_create) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}

# Print setup completion message
cat("Setup completed successfully!\n")
cat("The following packages are now available:\n")
print(sessionInfo())

# Print directory structure
cat("\nDirectory structure created:\n")
for (dir in dirs_to_create) {
  cat("-", dir, "\n")
}

# Save session info to file
sink("reports/models/session_info.txt")
print(sessionInfo())
sink() 