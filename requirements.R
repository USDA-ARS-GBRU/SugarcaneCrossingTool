# Install required packages if not already installed
required_packages <- c(
  # Core Shiny packages
  "shiny",
  "DT",
  "shinyBS",
  "shinyjs",
  "shinydashboard",
  "shinyWidgets",
  
  # Data manipulation and analysis
  "dplyr",
  "tidyr",
  "stringr",
  "reshape2",
  "purrr",
  "magrittr",
  
  # Visualization
  "plotly",
  "ggplot2",
  "visNetwork",
  "networkD3",
  "viridis",
  
  # File handling and data formats
  "jsonlite",
  "writexl",
  "readxl",
  
  # Domain specific
  "brapi",
  "SimpleMating",
  
  # Utilities
  "sortable",
  "plyr"
)

# Function to install missing packages from CRAN
install_missing_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    message("Installing the following packages: ", paste(new_packages, collapse=", "))
    install.packages(new_packages, dependencies=TRUE)
  }
}

# Function to install packages from Bioconductor if needed
install_bioc_packages <- function() {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  # Add any Bioconductor packages here if needed
  # BiocManager::install(c("package1", "package2"))
}

# Function to install packages from GitHub if needed
install_github_packages <- function() {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  # Add any GitHub packages here if needed
  # devtools::install_github("username/repository")
}

# Main installation process
tryCatch({
  # Install CRAN packages
  install_missing_packages(required_packages)
  
  # Install Bioconductor packages
  install_bioc_packages()
  
  # Install GitHub packages
  install_github_packages()
  
  # Load all required packages
  invisible(lapply(required_packages, library, character.only = TRUE))
  
  # Print package versions for documentation
  cat("\nInstalled package versions:\n")
  for(pkg in required_packages) {
    cat(sprintf("%s: %s\n", pkg, packageVersion(pkg)))
  }
  
  cat("\nAll required packages have been installed and loaded successfully!\n")
  
}, error = function(e) {
  cat("\nError during package installation:\n", conditionMessage(e), "\n")
  cat("Please check your internet connection and try again.\n")
})

# Save package state with renv if available
if(requireNamespace("renv", quietly = TRUE)) {
  renv::snapshot()
  cat("\nPackage state has been saved to renv.lock\n")
} 