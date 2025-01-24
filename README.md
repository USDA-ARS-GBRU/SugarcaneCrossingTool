
# Sugarcane Crossing Tool (SCT)

A Shiny application for managing sugarcane breeding crosses, powered by BrAPI and BreedBase.

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

[Add your license information here]

## Support

[Add support contact information here]

## Acknowledgments

- USDA-ARS
- BreedBase team
- BrAPI community

When contributing, please adhere to the coding style and conventions used in the existing codebase. Provide clear and concise commit messages and include any necessary documentation or comments to explain your changes. 

If you encounter any issues or have suggestions for improvements, please open an issue on the GitHub repository. 

Remember to regularly sync your forked repository with the main repository to keep it up to date with the latest changes. 

