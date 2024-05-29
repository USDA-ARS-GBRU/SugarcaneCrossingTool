
**Sugarcane Crossing Tool**

-----------------------------------

**Demo site here (no login):** 
https://keocorak.shinyapps.io/SCT/

**Sugarcane Crossing Tool (SCT)** 

**For Users/Breeders Logging into the App** 

1. Open the SCT application in your web browser. 
1. On the left sidebar, select your location from the "Select Location" dropdown menu. 
1. Enter your CID (Crossing ID) in the "Login with your CID" text input field. 
1. Choose a date from the calendar for which you want to view the flowering inventory data. 

**Using the App** 

1. After logging in and selecting a date, click the "Get Flower Inventory Data" button to pull the inventory data from the BrAPI database. 
1. Once the data is loaded, you can navigate through different tabs on the left sidebar to access various features: 
- **Home**: Provides an overview and instructions on how to use the app. 
- **Flowering Inventory**: Displays a table showing the count and sex of each clone that is flowering on the selected date. 
- **Kinship/Pedigree**: Allows you to view the pedigree table, relationship matrix, and visualize pedigrees of the flowering clones. 
- **Clone Performance**: Shows the performance data for each flowering clone, including mean and standard deviation of various traits. You can also view a scatter plot of selected traits. 
- **Previous Crosses**: Displays a table of previous crosses made with the flowering clones, including the number of progeny produced from each cross. 
- **Download Data**: Enables you to download a full data report as an Excel file, including inventory, pedigree, performance, and cross data. 
3. Explore the different tabs and utilize the available features to analyze and make informed decisions about your sugarcane crossing experiments. 

Note: Make sure to click the "Get Flower Inventory Data" button each time you choose a new date to update the displayed data. 

**For Developers** 

**Connection to a BrAPI-compliant database like Breedbase**

Create a .Renviron file in your working directory

```
cd /path/to/working/directory
touch .Renviron
```

Then add the following two lines to the .Renviron file, replaced the text in quotes with your username and password

```
USER="yourbreedbaseusername"
PASSWORD="yourbreedbasepassword"
```

**Implementation with BrAPI Library** 

The SCT application utilizes the BrAPI library in R to interact with the BrAPI (Breeding API) database. The BrAPI library provides functions to connect to the database, retrieve data, and perform various operations. 

To establish a connection to the BrAPI database, the as.ba\_db() function from the BrAPI library is used. The connection details, such as the database URL, user credentials, and other parameters, are specified in the app\_configs.R file. 

The app makes use of several BrAPI functions to retrieve data from the database, such as ba\_studies\_table(), ba\_germplasm\_details2(), ba\_germplasm\_progeny(), ba\_phenotypes\_search(), and ba\_crosses\_study(). These functions are called within the app to fetch the required data based on user inputs and selections. 

**Code Structure** 

The SCT application follows a modular code structure using the Shiny framework in R. The main components of the code are: 

- **app.R**: The main application file that defines the user interface (UI) and server logic. 
- **app\_functions.R**: Contains custom functions used in the application, such 

  as PedMatrix(), InitCrossTable(), and createPedigreeGraph(). 

- **app\_configs.R**: Stores configuration settings, including the BrAPI database connection details and initial data loading. 
- **Flowering.R**: Contains functions and server logic related to the flowering inventory feature. 
- **Pedigree.R**: Includes functions and server logic for handling pedigree data and visualization. 
- **Performance.R**: Implements functions and server logic for clone performance data and visualization. 
- **Crosses.R**: Defines functions and server logic for managing and displaying previous crosses data. 
- **Download\_page.R**: Contains functions and server logic for downloading data reports. 

The UI is built using the dashboardPage() function from the bs4Dash library, which provides a structured layout with a header, sidebar, and body. The UI components, such as input controls and output displays, are defined within the dashboardBody() using various Shiny functions like tabItem(), box(), DTOutput(), and plotlyOutput(). 

The server logic is implemented in the server function in app.R, which sources the necessary files (Flowering.R, Pedigree.R, performance.R, crosses.R, download\_page.R) to include their respective functions and server logic. The server function handles user interactions, data retrieval, and rendering of outputs. It uses reactive expressions and observers to respond to user inputs and update the displayed data accordingly. 

**How to Contribute** 

To contribute to the SCT application, follow these steps: 

1. Fork the repository on GitHub. 
1. Clone your forked repository to your local machine. 
1. Create a new branch for your feature or bug fix. 
1. Make the necessary changes and additions to the codebase.  
- If you are modifying a specific feature (e.g., flowering inventory, pedigree, performance, crosses, or download), make sure to update the corresponding file (Flowering.R, Pedigree.R, performance.R, crosses.R, or download\_page.R). 
- If you are adding a new feature, create a new file for it and include the necessary functions and server logic. 
5. Test your changes thoroughly to ensure they work as intended. 
5. Commit your changes and push them to your forked repository. 
5. Submit a pull request to the main repository, describing your changes and their purpose. 

When contributing, please adhere to the coding style and conventions used in the existing codebase. Provide clear and concise commit messages and include any necessary documentation or comments to explain your changes. 

If you encounter any issues or have suggestions for improvements, please open an issue on the GitHub repository. 

Remember to regularly sync your forked repository with the main repository to keep it up to date with the latest changes. 

