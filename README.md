
# Sugarcane Crossing Tool (SCT)

A Shiny application for managing sugarcane breeding crosses, powered by BrAPI and BreedBase.

## Authors

### Lead Developers
- **Keo Corak**
  - USDA-ARS GBRU
  - GitHub: [keocorak](https://github.com/keocorak)
    
- **Romil Shah** 
  - North Carolina State University
  - Email: rmshah3@ncsu.edu
  - GitHub: [romil2807](https://github.com/romil2807)

### Collaborators
We thank all contributors who have participated in this project. Special thanks to:
- USDA-ARS Genomics and Bioinformatics Research Unit (GBRU)

## Overview

The Sugarcane Crossing Tool (SCT) helps breeders manage and optimize their crossing programs by:
- Tracking flowering inventory
- Analyzing pedigree relationships
- Evaluating clone performance
- Planning and optimizing crosses
- Managing crossing cubicles

## Getting Started

### For Breeders

1. Access the application through your web browser
2. From the left sidebar:
   - Select your location
   - Select your breeder ID
   - Choose a date
   - Click "Get Flower Inventory"

### Features

#### Inventory Management
- Track male and female flowering parents
- Sort parents into crossing groups
- View flowering locations and counts

#### Pedigree Analysis
- View relationship matrices
- Interactive pedigree visualizations
- Track progeny counts

#### Performance Data
- View clone performance metrics
- Interactive trait scatter plots
- Customizable trait comparisons

#### Cross Planning
- View historical crosses
- Access reciprocal cross information
- Optimize crossing plans based on multiple criteria

#### Cubicle Management
- Organize crosses into breeding cubicles
- Track pollination and processing dates
- Manage male:female ratios
- Export cubicle layouts

## Dependencies

### Automatic Installation

To install all required packages, run:
R
source("requirements.R")
This will:
- Install any missing required packages
- Load all necessary libraries
- Print installed package versions
- Update the renv lockfile if renv is being used

### Manual Installation

If you prefer to install packages manually, the following packages are required:

Core Shiny packages:
- shiny, DT, shinyBS, shinyjs, shinydashboard, shinyWidgets

Data manipulation and analysis:
- dplyr, tidyr, stringr, reshape2, purrr, magrittr

Visualization:
- plotly, ggplot2, visNetwork, networkD3, viridis

File handling and data formats:
- jsonlite, writexl, readxl

Domain specific:
- brapi, SimpleMating

Utilities:
- sortable, plyr

### System Requirements

- R version 4.0.0 or higher
- Internet connection for package installation
- Write permissions to R library location

### Troubleshooting

If you encounter any issues during package installation:
1. Check your internet connection
2. Ensure you have write permissions to your R library location
3. Check the error message in the console for specific package installation failures
4. Make sure your R version meets the minimum requirements

For any persistent issues, please check the error messages in the console and consult the package documentation for specific requirements.

## Installation

### Prerequisites
- R (>= 4.0.0)
- RStudio (recommended)
- Access to a BreedBase instance

### Setup

1. Clone the repository:
bash
git clone https://github.com/USDA-ARS-GBRU/SugarcaneCrossingTool.git

2. Install dependencies:
R
install.packages("renv")
renv::restore()

3. Configure database connection:
bash
Create .Renviron in home directory
echo 'URL="yourbreedbaseurl"
USERNAME="yourbreedbaseusername"
PASS="yourbreedbasepassword"' > ~/.Renviron

4. Update `app_configs.R` with your:
- Location IDs
- Crossing project IDs
- Required data files

## Usage

1. Launch the app in RStudio or run:
R
shiny::runApp()
2. Select your location and breeder ID
3. Choose a date and get flower inventory
4. Use the sidebar to navigate between features

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

## License

GNU-GPL3

## Support

Address questions via github issues page or by contacting keo.corak at usda.gov

## Acknowledgments

- USDA-ARS
- BreedBase team
- BrAPI community

When contributing, please adhere to the coding style and conventions used in the existing codebase. Provide clear and concise commit messages and include any necessary documentation or comments to explain your changes. 

If you encounter any issues or have suggestions for improvements, please open an issue on the GitHub repository. 

Remember to regularly sync your forked repository with the main repository to keep it up to date with the latest changes. 

