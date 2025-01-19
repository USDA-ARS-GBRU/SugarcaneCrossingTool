# app.R

# INIT -----------------

## LIBRARIES -----------
# Load required libraries

library(brapi)
library(tidyverse)
library(shiny)
library(bs4Dash)
library(DT)
library(rjson)
library(reshape2)
library(AGHmatrix)
library(heatmaply)
library(shinyWidgets)
library(data.table)
library(writexl)
library(tis)
library(fresh)
library(networkD3)
library(visNetwork)
library(bslib)
library(SimpleMating)
library(sortable)
library(shinyjs)
library(plotly)
library(openxlsx) 
library(writexl)


# Include necessary JavaScript libraries

tags$head(
  tags$script(src = "https://d3js.org/d3.v5.min.js"),
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/networkD3/0.4.1/networkD3.min.js")
)


## CUSTOM DATA and FUNCTION LOAD -----------
# Source custom functions and configurations from separate files
source("app_functions.R")
source("app_configs.R")
source("modules/flowering.R")
source("modules/pedigree.R")
source("modules/performance.R")
source("modules/crosses.R")
source("modules/download_page.R")


## THEME
#TBA

## CHECK if true connection
brapi::ba_check(brap) # should be true, for debugging

# USER INTERFACE  -------------------------------------------------------------

ui <- dashboardPage(
  

  title = "STC",
  dark = NULL,

  ## CONTROLBAR ----
  controlbar = dashboardControlbar(
    collapsed = TRUE,
    div(class = "p-3", skinSelector()),
    pinned = FALSE
  ),

  ## HEADER --------
  header = dashboardHeader(
    title = "SCT",
    rightUi = tagList(
      dropdownMenu(
        type = "notifications",
        badgeStatus = NULL,
        icon = icon("sun"),
        switchInput(
          inputId = "dark_mode",
          label = "Dark Mode",
          onStatus = "success",
          offStatus = "danger"
        )
      )
    )
  ),

  ## SIDEBAR ------
  sidebar = dashboardSidebar(
    selectInput("location", "Step 1: Select Location", choices = location_iid_map),
    
    #this is kind of confusing. The idea is that multiple breeders might be working at same location (Florida) and they should be able to track crosses independently, even though cane lines are combined
    #so crossesid refers to crosses a specific breeder is making
    selectInput("crossesid", "Step 2: Select Breeder", choices=crosses_iid_map), 
    
    dateInput(
      "date",
      "Step 3: Choose A Date",
      value = "2023-10-10"
    ),
    #actionButton("brapipull", "Get Flower Inventory Data"),
    textOutput("dateWarning"), 
    
    p("for testing, select:", strong("October 10, 2023")),
    
    actionButton(
      "brapipull",
     strong("Step 4. Get Flower Inventory")
    ),
    p("Don't forget to push 'Get Flower Inventory", strong("each"), "each time you choose a new date"),
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      menuItem("Inventory/Sorting",
               tabName = "flowering",
               icon = icon("seedling")
      ),
      menuItem("Kinship/Pedigree",
               tabName = "kinship",
               icon = icon("people-group")
      ),
      menuItem("Clone Performance",
               tabName = "performance",
               icon = icon("star")
      ),
      menuItem("Previous Crosses",
               tabName = "crosses",
               icon = icon("xmark")
      ),
      menuItem("(BETA) Cross Optimization",
               tabName = "optimization",
               icon = icon("dna")
      ),
      menuItem("Download Data",
               tabName = "download",
               icon = icon("download")
      ),
      menuItem("Cubicle Manager",
               tabName = "cubicle",
               icon = icon("th"))
      )
    ),

  ## BODY -----

  body = dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .rank-list-container .rank-list-item {
          color: #333;
          background-color: #f8f9fa;
        }
        .dark-mode .rank-list-container .rank-list-item {
          color: #f8f9fa;
          background-color: #343a40;
        }
        /* Updated styles for sidebar elements */
        .dark-mode .main-sidebar {
          background-color: #343a40 !important;
        }
        .dark-mode .main-sidebar .nav-sidebar .nav-item .nav-link,
        .dark-mode .main-sidebar .nav-sidebar .nav-item .nav-link p,
        .dark-mode .main-sidebar .brand-text,
        .dark-mode .main-sidebar .user-panel .info,
        .dark-mode .sidebar .form-group label,
        .dark-mode .sidebar p,
        .dark-mode .sidebar .btn-default,
        .dark-mode .sidebar .form-control,
        .dark-mode .sidebar .input-group-text,
        .dark-mode .sidebar .selectize-input,
        .dark-mode .sidebar .selectize-dropdown {
          color: #f8f9fa !important;
        }
        .dark-mode .sidebar .form-control,
        .dark-mode .sidebar .input-group-text,
        .dark-mode .sidebar .selectize-input,
        .dark-mode .sidebar .selectize-dropdown {
          background-color: #454d55 !important;
          border-color: #6c757d !important;
        }
        .dark-mode .sidebar .btn-default {
          background-color: #454d55 !important;
          border-color: #6c757d !important;
        }
        .dark-mode .sidebar .btn-default:hover {
          background-color: #5a6268 !important;
        }
        /* Styles for optimized crossing plan */
        .dark-mode .box-body {
          color: #f8f9fa !important;
        }
        .dark-mode .dataTables_wrapper {
          color: #f8f9fa !important;
        }
        .dark-mode .dataTables_wrapper .dataTables_length,
        .dark-mode .dataTables_wrapper .dataTables_filter,
        .dark-mode .dataTables_wrapper .dataTables_info,
        .dark-mode .dataTables_wrapper .dataTables_processing,
        .dark-mode .dataTables_wrapper .dataTables_paginate {
          color: #f8f9fa !important;
        }
        .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button {
          color: #f8f9fa !important;
        }
        .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.current,
        .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
          color: #333 !important;
        }
        /* Consistent tab styling */
        .nav-tabs .nav-link.active {
          background-color: #007bff !important;
          color: #ffffff !important;
          border-color: #007bff !important;
        }

        .nav-tabs .nav-link {
          color: #007bff !important;
        }

        .nav-tabs .nav-link:hover:not(.active) {
          border-color: #e9ecef #e9ecef #dee2e6;
          color: #0056b3 !important;
        }

        /* Ensure sidebar consistency */
        .nav-sidebar .nav-item .nav-link.active {
          background-color: #007bff !important;
          color: #ffffff !important;
        }

        .nav-sidebar .nav-item .nav-link {
          color: #333333 !important;  /* Changed to black */
        }

        .nav-sidebar .nav-item .nav-link:hover:not(.active) {
          color: #007bff !important;
        }

        /* Dark mode compatibility */
        .dark-mode .nav-tabs .nav-link.active {
          background-color: #375a7f !important;
          color: #ffffff !important;
          border-color: #375a7f !important;
        }

        .dark-mode .nav-tabs .nav-link {
          color: #375a7f !important;
        }

        .dark-mode .nav-sidebar .nav-item .nav-link.active {
          background-color: #375a7f !important;
          color: #ffffff !important;
        }

        .dark-mode .nav-sidebar .nav-item .nav-link {
          color: #f8f9fa !important;  /* Keep light color for dark mode */
        }
      "))
    ),
    tags$script("
      $(document).ready(function() {
        $('a[data-value=\"welcome\"]').tab('show');
      });
    "),
    tabItems(

      ### Home content ----
      tabItem(
        tabName = "home",
        
        h1("Sugarcane Integrated Breeding System (SIBS) Sugarcane Crossing Tool (SCT)"),
        p("Welcome to SCT! To use this app, follow the instructions below:"),
        p("* Start by loggin in: from the sidebar on the left, (1) select a location and (2) breeder name."),
        p("* Then, chose an (3) inventory date."),
        p("* Next, (4) Click on the 'Get Flower Inventory' button"),
        p("* After that, move to the Flowering Inventory tab to view your data and sort flowering clones by gender"),
        p("* You can then click on other tabs to explore related breeding data"),
        p("Follow ", a(href="https://github.com/USDA-ARS-GBRU/SugarcaneCrossingTool", "this link"), " to the github repo for detailed instructions."),
       
        card(
          
        card_header("Login Information"),
        
        
        p("You've logged in to view inventory for this location: "),
        
        span(textOutput("inventoryPointer"), style="color:blue"),
        
        br(),
        
        p("You've logged in as User:"),
        
        span(textOutput("crossPointer"), style="color:blue")),
        
        card(
          card_header("Inventory Information"),
          
          p("You're viewing inventory for this day:"),
          span(textOutput("datePointer"), style="color:blue"))),

      ### Inventory content -----
      tabItem(
        tabName = "flowering",
        
        fluidRow(
       
     
          box(
            title = "Step 5: Sorting",
            p("Drag and drop the available flowering clones into their appropriate category.", strong("Only"), "sorted clones will be displayed in subsequent tabs and/or used in cross prediction so this step must be done first."),
            width = 12,
            fluidRow(
              column(
                width = 4,
                h4("Available Clones"),
                uiOutput("available_clones")
              ),
              column(
                width = 4,
                h4("Female Parents"),
                uiOutput("female_parents")
              ),
              column(
                width = 4,
                h4("Male Parents"),
                uiOutput("male_parents")
              )
            )
          )
        ),
     

        box(title="This table shows you the raw data for the sorting you did above.",
             textOutput("dataSourceText"),
            width=12,
            fluidRow(
              column(
                width=4, 
                DTOutput("inventoryTable"),
              )
            )),
        
       


        
         ),

      ### Pedigree tab content ----

      tabItem(
        tabName = "kinship",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Pedigree Table",
            fluidRow(
              box(
                title="Basic Pedigree Information",
                width=12,
                actionButton(
                  inputId = "makepedigree",
                  label = "Get Pedigree Data"
                ),
                p("This table shows you the pedigree of each", strong("sorted"), "and flowering clone and the number of progeny it produced")
              ),
              DTOutput("pedigreeTable"),
            )
          ),
          tabPanel(
            "Relationship Matrix",
            box(
              title="Relationship Heatmap",
              width=12,
              p("This is a relationship matrix of the flowering clones. Values closer to one indicate high relatedness. You can zoom in to particular regions of the matrix.")),
            plotlyOutput("pedigreeMatrix")
          ),
          tabPanel(
            "Visualize Pedigrees",
            fluidRow(
              box(
                title="Pedigree Trees",
                p("You can select clones from the drop-down menu to see their pedigree. Note- future work will allow you to select how many generations you want to see"),
                width=12,
                uiOutput("cloneDropdown"),
                visNetworkOutput("pedigreeGraph")
              )
            )
          )
        )
      ),

      ### Performance tab content ----
      tabItem(
        tabName = "performance",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Performance Table",
            fluidRow(
              box(
                width=12,
                actionButton(
                  inputId = "makeperformance",
                  label = "Get Performance Data"
                ),
                p("This table shows the mean and sd of the performance for each clone 
                  that is flowering on the date you selected. Once data has been pulled from the database, you will be able to select traits to view using the drop-down menu. This step may take several minutes, please be patient.", 
                  strong("Note, this data is very roughly averaged, please interpret with caution.")),
                
                
                # stuff for what phenotype to select
                uiOutput(outputId = "colSelect"), # render html list output
                actionButton("selectCol", "View Selected Data")
              )
            ),
            DTOutput("performanceTable")
          ),
          tabPanel(
            "Trait Scatter Plot",
            fluidRow(
              box(
                width=12,
                uiOutput("scatterPlotDropdown_x"),
                uiOutput("scatterPlotDropdown_y"),
                plotlyOutput("traitScatterPlot")
              )
            )
          )
        )
      ),
      
      ### Crosses tab content ----
      tabItem(
        tabName = "crosses",
        fluidRow(
          box(
            title="Previous crosses made with selected clones",
            width=12,
            actionButton(
              inputId = "makecrosses",
              label = "Get Data on Previous Crosses and Seedlots"
            ),
            p("This table shows a count of", strong("previous"), "made with your selected clones. It also shows the total number of progeny and availablity of seedlots for these crosses. This table is filtered based on categorizatons made in step 5.
              If the cross was made earlier this year, the 'Progeny.Per.Cross' column will read 'None yet, new cross this year'.")
          )
        ),
        DTOutput("crossesTable")
      ),

  

      
      #### Download tab content ----
      tabItem(
  tabName = "download",
  fluidRow(
    box(
      title = "Download Full Data Report",
      p("This button will allow you to download a full data report as an excel file.
        A partial download will fail, so make sure you've pulled all the inventory, pedigree, performance and cross data.
        A successful download will have a date in the file name."),
      downloadButton("downloaddata", "Download Full Data Report")
    ),
    box(
      title = "Download Optimized Crossing Plan",
      p("Click the button below to download the optimized crossing plan as an Excel file."),
      downloadButton("download_optimized_plan", "Download Optimized Crossing Plan")
    ),
    box(
      title = "Download Cubicle Layout",
      p("Download the current cubicle assignments and layout as an Excel file."),
      downloadButton("download_cubicle_layout", "Download Cubicle Layout")
    )
  )
),

      ### Cross Optimization tab content ----
      tabItem(
        tabName = "optimization",
        p("BETA implementation of", a(href="https://github.com/Resende-Lab/SimpleMating", "SimpleMating R package"), 
          "This optimizes the midparent value of potential cross combinations based on a weighted selection index of parental BVs."),
        fluidRow(
          box(
            title = "Optimization Parameters",
            width = 6,
            numericInput("n_crosses", "Number of Crosses to Select:", 10, min = 1, max = 100),
            numericInput("max_crosses_per_parent", "Max Crosses per Parent:", 3, min = 1, max = 10),
            sliderInput("culling_k", "Culling Pairwise K:", 
                       min = 0, max = 1, value = 0.5, step = 0.05),
            
            p(strong("Trait Weights (must sum to 1):")),
            sliderInput("brix", "Average Brix:", 
                       min = 0, max = 1, value = 0.33, step = 0.01),
            sliderInput("biomass", "Total Biomass:", 
                       min = 0, max = 1, value = 0.33, step = 0.01),
            sliderInput("ratoon", "Ratooning Ability:", 
                       min = 0, max = 1, value = 0.34, step = 0.01),
            
            textOutput("weight_sum_warning"),
            actionButton("run_optimization", "Run Optimization")
          ),
          box(
            title = "Optimized Crossing Plan",
            width = 6,
            DTOutput("optimized_crosses_table")
          )
        ),
        fluidRow(
          box(
            title = "Optimization Visualization",
            width = 12,
            plotlyOutput("optimization_plot")
          )
        )
      ),

      tabItem(
        tabName = "cubicle",
        fluidRow(
          box(
            title = "Cubicle Management",
            width = 12,
            p("Organize your optimized crosses into breeding cubicles. Each cubicle can contain up to 3 crosses with the same male parent."),
            actionButton("create_cubicle", "Create Cubicle from Selected Crosses", 
                        class = "btn btn-primary"),
            actionButton("print_layout", "Print Layout", 
                        class = "btn btn-info"),
            downloadButton("save_data", "Save Layout"),
            fileInput("load_data", "Load Layout"),
            hr(),
            helpText(class = "help-text", "1. Select up to 3 crosses with the same male from the table below"),
            helpText(class = "help-text", "2. Click 'Create Cubicle' to group them"),
            helpText(class = "help-text", "3. Track your progress in the summary"),
            
            dateInput("pollination_date", "Pollination Date:", value = Sys.Date()),
            dateInput("processing_date", "Processing Date:", value = Sys.Date() + 60)
          )
        ),
        fluidRow(
          column(8,
            box(
              title = "Available Crosses",
              width = 12,
              DTOutput("crossing_table")
            ),
            box(
              title = "Parent Usage Summary",
              width = 12,
              DTOutput("parent_summary_table")
            )
          ),
          column(4,
            box(
              title = "Cubicle Layout",
              width = 12,
              DTOutput("cubicle_table"),
              uiOutput("cubicle_ratios")  # New UI element for ratios
            ),
            box(
              title = "Summary Statistics",
              width = 12,
              verbatimTextOutput("statistics")
            )
          )
        )
      ),

      tabItem(
        tabName = "welcome",
        fluidRow(
          box(
            title = "Welcome to the Sugarcane Integrated Breeding System (SIBS) Sugarcane Crossing Tool (SCT)",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            p("Welcome to SCT! This application helps manage and optimize your breeding program. Follow these steps to get started:"),
            
            h4("Step 1: Initial Setup"),
            tags$ol(
              tags$li(strong("Login Information:"),
                tags$ul(
                  tags$li("Select a location from the sidebar"),
                  tags$li("Choose your breeder name"),
                  tags$li("Select an inventory date")
                )
              ),
              tags$li(strong("Get Inventory Data:"),
                tags$ul(
                  tags$li("Click 'Get Flower Inventory' button"),
                  tags$li("View your data in the Flowering Inventory tab"),
                  tags$li("Sort flowering clones by gender")
                )
              ),
              tags$li(strong("Parent Assignment:"),
                tags$ul(
                  tags$li("Move to the Inventory/Sorting tab"),
                  tags$li("Assign roles (male/female) to parents"),
                  tags$li("Review parent statistics")
                )
              ),
              tags$li(strong("Cross Optimization:"),
                tags$ul(
                  tags$li("Navigate to Cross Beta Optimization tab"),
                  tags$li("Set number of desired crosses"),
                  tags$li("Adjust maximum crosses per parent"),
                  tags$li("Set kinship threshold"),
                  tags$li("Adjust trait weights (must sum to 1)"),
                  tags$li("Run optimization"),
                  tags$li("Review results in plot and table")
                )
              ),
              tags$li(strong("Cubicle Management:"),
                tags$ul(
                  tags$li("Go to Cubicle Manager tab"),
                  tags$li("Select crosses from optimization results"),
                  tags$li("Create cubicles (max 3 females per male)"),
                  tags$li("Set planting and flowering dates"),
                  tags$li("Add notes as needed"),
                  tags$li("Monitor female-to-male ratios"),
                  tags$li("Download or print layout")
                )
              )
            ),
            
            hr(),
            
            h4("Important Notes:"),
            tags$ul(
              tags$li("Always click 'Get Flower Inventory' when changing dates"),
              tags$li("Parent assignments can be modified at any time"),
              tags$li("Optimization parameters can be adjusted as needed"),
              tags$li("Monitor female-to-male ratios in cubicles"),
              tags$li("Save your work using the download options")
            ),
            
            hr(),
            
            div(
              class = "alert alert-info",
              icon("info-circle"), 
              " For detailed information about each feature, look for the help icons (?) in each section."
            )
          )
        )
      )
    )
  )
)


# Define the inventory_init function as a global variable
inventory_init <<- eventReactive(input$brapipull, withProgress(message = "Pulling Inventory Data", {
  tryCatch({
    inven <- data.frame(brapi::ba_studies_table(con = brap, studyDbId = reactive_iid(), rclass="data.frame")) %>%
      filter(observationLevel == "plant") %>% # select just plant rows
      set_names(~(.)%>% str_replace_all("SUGARCANE.*","") %>% str_replace_all("\\.","")) %>%  # take CO term out of colnames
      filter(FloweringTime== reactive_date()) %>% 
      select(germplasmName, germplasmDbId, SexMFWM) %>% 
      group_by(germplasmName, germplasmDbId, SexMFWM) %>% 
      summarise(count = n()) %>%
      rename(Clone = germplasmName, FloweringCount = count, Sex = SexMFWM)
    dataSource("Data pulled from BrAPI")
    inven
  }, error = function(e) {
    dataSource("Saved data is being rendered")
    data.frame(Clone = character(), FloweringCount = numeric(), Sex = character()) # Return an empty data frame with the expected columns
  })
}))

# SERVER ---------------------------------------

server <- function(input, output, session) {
  library(networkD3)
  
  # Reactive value for selected date
  reactive_date <- reactive({
    input$date
  })

  # Reactive value for data source
  dataSource <- reactiveVal()
  
  # Reactive values for selected columns in performance tab
  rv <- reactiveValues(selectedColumns = NULL)
  rv_trait_scatter <- reactiveValues(selectedColumns = NULL)

  # Reactive value for selected clone
  selectedClone <- reactiveVal()

  selected_clone <- reactive({
    req(input$selectedClone)
    input$selectedClone
  })

  # Update selectedColumns when the user selects new columns in the Performance tab
  observeEvent(input$selectCol, {
    rv$selectedColumns <- input$phenoPick
    rv_trait_scatter$selectedColumns <- input$phenoPick
  })


  # Update selected clone when user selects a clone in pedigree tab
  observeEvent(input$selectedClone, {
    selectedClone <- input$selectedClone
    updateSelectInput(session, "pedigreeGraphUI")
  })

  # Update X-axis and Y-axis dropdowns in performance scatter plot based on selected phenotypes
  observe({
    phenotypes <- input$phenoPick

    # Update X-axis dropdown
    updateSelectInput(session, "xAxis_scatter", choices = phenotypes, selected = phenotypes[1])

    # Update Y-axis dropdown
    updateSelectInput(session, "yAxis_scatter", choices = phenotypes, selected = phenotypes[2])
  })

  # Update selected columns for scatter plot when user selects new columns
  observeEvent(input$selectCol_scatter, {
    rv_trait_scatter$xAxis <- input$xAxis_scatter
    rv_trait_scatter$yAxis <- input$yAxis_scatter
  })

  observeEvent(input$selectCol_scatter, {
    rv_trait_scatter$selectedColumns <- input$phenoPick_scatter
  })

  # Reactive value for selected location
  reactive_iid <- reactive({
    as.character(input$location)
  })
  # Reactive value for selected cross ID
  reactive_cid <- reactive({as.character(input$crossesid)})
  
  # Add reactiveValues for sharing data between modules
  rv <- reactiveValues(
    optimization_result = NULL,
    previous_crosses = NULL
  )
  
  # Add the renderText for dataSourceText
  output$dataSourceText <- renderText({
    dataSource()
  })
  
  # Call the server functions from separate files
  inventory_init <- flowering_server(input, output, session, reactive_date, reactive_iid, dataSource )
  pedigree_server(input, output, session, reactive_iid, selectedClone, inventory_init, clone_assignments)
  performance_server(input, output, session, reactive_iid, rv, rv_trait_scatter, inventory_init, clone_assignments)
  crosses_server(input, output, session, reactive_cid, inventory_init, clone_assignments, rv)
  download_page_server(input, output, session, reactive_date)
  
  # Output for inventory pointer
  output$inventoryPointer <- renderText({
    location <- names(location_iid_map)[location_iid_map == input$location]
    paste("Location:", location, "-", unique(brapi::ba_studies_table(con = brap, studyDbId = input$location)$studyName))
  })
  # Output for cross pointer
  output$crossPointer <- renderText({
    validate(
      need(input$crossesid != "", "Please chose a breeder login:")
    )
    crosses <- names(crosses_iid_map)[crosses_iid_map == input$crossesid]
    paste("Breeder:", crosses, "-", unique(ba_crosses_study(con = brap2, crossingProjectDbId = input$crossesid, rclass = "data.frame")$data.crossingProjectName[[1]])) #crossing project name has a breedbase bug- should return text, not number
    
    
  })

  #output for inventory date pointer 
  output$datePointer <- renderText({
    # validate(
    #   need(input$date != "", "Please chose a date")
    # )
    
    paste(as.character(input$date))
    
  })
    
 # Reactive value to store the current state of clone assignments
  clone_assignments <- reactiveVal(list(available = character(), male = character(), female = character()))
  
  # Initialize available clones
  observe({
    inventory_data <- inventory_init()
    clones <- unique(inventory_data$Clone)
    clone_assignments(list(available = clones, male = character(), female = character()))
  })
  
  # Render sortable lists
  output$available_clones <- renderUI({
    bucket_list(
      header = "Available Clones",
      group_name = "clone_buckets",
      orientation = "vertical",
      add_rank_list(
        text = "Drag clones from here",
        labels = clone_assignments()$available,
        input_id = "available_list"
      )
    )
  })
  
  output$male_parents <- renderUI({
    bucket_list(
      header = "Male Parents",
      group_name = "clone_buckets",
      orientation = "vertical",
      add_rank_list(
        text = "Drag male parents here",
        labels = clone_assignments()$male,
        input_id = "male_list"
      )
    )
  })
  
  output$female_parents <- renderUI({
    bucket_list(
      header = "Female Parents",
      group_name = "clone_buckets",
      orientation = "vertical",
      add_rank_list(
        text = "Drag female parents here",
        labels = clone_assignments()$female,
        input_id = "female_list"
      )
    )
  })
  
  # Update clone assignments when lists change
  observe({
    clone_assignments(list(
      available = input$available_list,
      male = input$male_list,
      female = input$female_list
    ))
  })
  
  # Initialize crossing plan from optimization results
  observe({
    # Get the optimized crosses from the optimization module
    opt_result <- optimize_crosses(
      inventory_data = inventory_init(), 
      male_parents = clone_assignments()$male,
      female_parents = clone_assignments()$female,
      n_crosses = input$n_crosses,
      max_crosses_per_parent = input$max_crosses_per_parent,
      culling_k = input$culling_k,
      prop_sel = input$prop_sel,
      blup = blup_data[,1:4],
      amat = as.matrix(parent_amat),
      weights = c(input$brix, input$biomass, input$ratoon)
    )
    
    # Check if optimization was successful and has crosses
    if (!is.null(opt_result) && !is.null(opt_result$crosses) && nrow(opt_result$crosses) > 0) {
      plan_data <- opt_result$crosses
      plan_data$status <- "Unassigned"
      plan_data$cubicle_id <- NA
      crossing_plan(plan_data)
    }
  })

  # Modify the optimization event handler to store results
  observeEvent(input$run_optimization, {
    # Get current inventory data
    inventory_data <- inventory_init()
    
    # Get selected parents
    male_parents <- clone_assignments()$male
    female_parents <- clone_assignments()$female
    
    # Check if parents are selected
    if (length(male_parents) == 0 || length(female_parents) == 0) {
      showNotification("Please select both male and female parents before running optimization.", type = "error")
      return()
    }
    
    # Run optimization
    rv$optimization_result <- tryCatch({
      optimize_crosses(
        inventory_data = inventory_data, 
        male_parents = male_parents,
        female_parents = female_parents,
        n_crosses = input$n_crosses,
        max_crosses_per_parent = input$max_crosses_per_parent,
        culling_k = input$culling_k,
        prop_sel = input$prop_sel,
        blup = blup_data[,1:4],
        amat = as.matrix(parent_amat),
        weights = c(input$brix, input$biomass, input$ratoon)
      )
    }, error = function(e) {
      showNotification(paste("Error in optimization:", e$message), type = "error")
      return(NULL)
    })
    
    # Update table output
    output$optimized_crosses_table <- renderDT({
      req(rv$optimization_result)
      crosses <- rv$optimization_result$crosses
      
      if (!is.null(crosses) && nrow(crosses) > 0) {
        # Join with previous crosses if available
        if (!is.null(rv$previous_crosses)) {
          crosses <- crosses %>%
            left_join(rv$previous_crosses, 
                     by = c("Female.Parent", "Male.Parent"))
        }
        
        # Add rank column
        crosses <- crosses %>%
          mutate(Rank = row_number())
        
        datatable(crosses, 
                 options = list(
                   scrollX = TRUE,
                   fixedColumns = list(leftColumns = 3),
                   pageLength = 10
                 ))
      } else {
        datatable(data.frame(Message = "No crosses found or error occurred"), 
                 options = list(pageLength = 10))
      }
    })
  })

  # Add weight sum warning
  output$weight_sum_warning <- renderText({
    total_weight <- input$brix + input$biomass + input$ratoon
    if (abs(total_weight - 1) > 0.01) {
      return(paste("Warning: Weights sum to", round(total_weight, 2), "- should equal 1"))
    } else {
      return(paste("Weights sum to", round(total_weight, 2)))
    }
  })

  # Store crossing plan data
  crossing_plan <- reactiveVal(NULL)

  # Display crossing plan table
  output$crossing_table <- renderDT({
    req(crossing_plan())
    datatable(
      crossing_plan(),
      selection = 'multiple',
      options = list(
        pageLength = 10,
        searching = TRUE,
        ordering = TRUE
      )
    )
  })

  # Store cubicle data
  cubicles <- reactiveVal(list())

  # Create new cubicle
  observeEvent(input$create_cubicle, {
    req(crossing_plan())
    selected_rows <- input$crossing_table_rows_selected
    
    if (length(selected_rows) == 0) {
      showNotification("Please select crosses first", type = "error")
      return()
    }
    
    selected_crosses <- crossing_plan()[selected_rows, ]
    
    # Check if all selected crosses have the same male parent
    if (length(unique(selected_crosses$Male.Parent)) > 1) {
      showNotification("All selected crosses must have the same male parent", type = "error")
      return()
    }
    
    # Show modal for custom cubicle ID
    showModal(modalDialog(
      title = "Create New Cubicle",
      textInput("custom_cubicle_id", "Enter Cubicle ID (optional)", 
                value = paste0("C", length(cubicles()) + 1)),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_cubicle", "Create")
      )
    ))
    
    # Store selected crosses temporarily
    rv$temp_selected_crosses <- selected_crosses
  })

  # Handle cubicle creation confirmation
  observeEvent(input$confirm_cubicle, {
    req(rv$temp_selected_crosses)
    
    # Create new cubicle with custom or default ID
    new_cubicle <- list(
      id = input$custom_cubicle_id,
      male = rv$temp_selected_crosses$Male.Parent[1],
      crosses = rv$temp_selected_crosses,
      pollination_date = input$pollination_date,
      processing_date = input$processing_date,
      notes = ""
    )
    
    # Update cubicles
    current_cubicles <- cubicles()
    current_cubicles[[length(current_cubicles) + 1]] <- new_cubicle
    cubicles(current_cubicles)
    
    # Update crossing plan status
    plan_data <- crossing_plan()
    selected_rows <- which(plan_data$Female.Parent %in% rv$temp_selected_crosses$Female.Parent &
                          plan_data$Male.Parent == rv$temp_selected_crosses$Male.Parent[1])
    plan_data$status[selected_rows] <- "Assigned"
    plan_data$cubicle_id[selected_rows] <- new_cubicle$id
    crossing_plan(plan_data)
    
    removeModal()
  })

  # Display cubicle table
  output$cubicle_table <- renderDT({
    current_cubicles <- cubicles()
    
    if (length(current_cubicles) == 0) {
      return(NULL)
    }
    
    cubicle_df <- do.call(rbind, lapply(current_cubicles, function(cubicle) {
      data.frame(
        Cubicle_ID = cubicle$id,
        Male = cubicle$male,
        Females = paste(cubicle$crosses$Female.Parent, collapse = ", "),
        Pollination_Date = format(cubicle$pollination_date, "%Y-%m-%d"),
        Processing_Date = format(cubicle$processing_date, "%Y-%m-%d"),
        Notes = cubicle$notes,
        stringsAsFactors = FALSE
      )
    }))
    
    datatable(
      cubicle_df,
      editable = list(target = "cell", disable = list(columns = c(1, 2, 3, 4, 5))),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      )
    )
  })

  # Add this observer to handle note updates
  observeEvent(input$cubicle_table_cell_edit, {
    info <- input$cubicle_table_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    
    # Only process if it's the Notes column (column 6)
    if(j == 6) {
      current_cubicles <- cubicles()
      # Update the notes in the corresponding cubicle
      current_cubicles[[i]]$notes <- v
      cubicles(current_cubicles)
    }
  })

  # Display statistics
  output$cubicle_statistics <- renderText({
    plan_data <- crossing_plan()
    if (is.null(plan_data)) return("No crossing plan loaded")
    
    total_crosses <- nrow(plan_data)
    assigned_crosses <- sum(plan_data$status == "Assigned")
    remaining_crosses <- total_crosses - assigned_crosses
    
    paste0(
      "Total Crosses in Plan: ", total_crosses, "\n",
      "Assigned to Cubicles: ", assigned_crosses, "\n",
      "Remaining to Assign: ", remaining_crosses, "\n",
      "Number of Cubicles: ", length(cubicles()), "\n",
      "Progress: ", round(assigned_crosses/total_crosses * 100, 1), "%"
    )
  })

  # Update the optimization plot
  output$optimization_plot <- renderPlotly({
    req(rv$optimization_result)
    
    # Get the optimization result data
    opt_data <- rv$optimization_result$crosses
    
    if (is.null(opt_data)) return(NULL)
    
    # Add Selected column based on ranking
    n_selected <- input$n_crosses
    total_crosses <- nrow(opt_data)
    
    # Create Selected column with TRUE for top n_crosses and FALSE for the rest
    opt_data$Selected <- FALSE  # Initialize all as FALSE
    if (n_selected > 0 && n_selected <= total_crosses) {
      opt_data$Selected[1:n_selected] <- TRUE
    }
    
    # Create hover text
    hover_text <- paste(
      "Female:", opt_data$Female.Parent,
      "\nMale:", opt_data$Male.Parent,
      "\nSelection Index:", round(opt_data$Y, 3),
      "\nKinship:", round(opt_data$K, 3),
      "\nSelected:", opt_data$Selected
    )
    
    # Create ggplot
    p <- ggplot(opt_data, aes(x = K, y = Y)) +
      geom_point(aes(color = Selected), size = 3, alpha = 0.7) +
      scale_color_manual(values = c("FALSE" = "gray70", "TRUE" = "#1f77b4")) +
      geom_vline(xintercept = input$culling_k, linetype = "dashed") +
      theme_minimal() +
      labs(
        x = "Kinship Coefficient",
        y = "Selection Index",
        title = paste("Cross Optimization Plot (Top", n_selected, "Crosses Highlighted)"),
        color = "Selected Crosses"
      ) +
      aes(text = hover_text)
    
    # Convert to plotly
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })

  # Add this for the cubicle visualization
  output$crossing_plot <- renderPlotly({
    req(crossing_plan())
    plan_data <- crossing_plan()
    
    # Create the plot
    plot_ly(plan_data, 
            x = ~K,  # Using K from optimization
            y = ~Y,  # Using Y from optimization
            color = ~status,
            text = ~paste("Female:", Female.Parent,
                         "<br>Male:", Male.Parent,
                         "<br>Status:", status,
                         "<br>Index:", round(Y, 3),
                         "<br>Kinship:", round(K, 3)),
            type = "scatter",
            mode = "markers",
            marker = list(size = 10)) %>%
      layout(
        title = "Crossing Plan Status",
        xaxis = list(title = "Kinship Coefficient (K)"),
        yaxis = list(title = "Selection Index (Y)"),
        showlegend = TRUE
      )
  })

  # Add this to your server function
  output$download_cubicle_layout <- downloadHandler(
    filename = function() {
      paste("cubicle_layout_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      # Create a new workbook
      wb <- createWorkbook()
      
      # Add cubicle layout sheet
      addWorksheet(wb, "Cubicle Layout")
      current_cubicles <- cubicles()
      if (length(current_cubicles) > 0) {
        cubicle_df <- do.call(rbind, lapply(current_cubicles, function(cubicle) {
          data.frame(
            Cubicle_ID = cubicle$id,
            Male = cubicle$male,
            Females = paste(cubicle$crosses$Female.Parent, collapse = ", "),
            Planting_Date = format(cubicle$planting_date, "%Y-%m-%d"),
            Expected_Flowering = format(cubicle$expected_flowering, "%Y-%m-%d"),
            Notes = cubicle$notes,
            stringsAsFactors = FALSE
          )
        }))
        writeData(wb, "Cubicle Layout", cubicle_df)
      }
      
      # Add crossing plan sheet
      addWorksheet(wb, "Crossing Plan")
      if (!is.null(crossing_plan())) {
        writeData(wb, "Crossing Plan", crossing_plan())
      }
      
      # Add summary statistics sheet
      addWorksheet(wb, "Summary")
      plan_data <- crossing_plan()
      if (!is.null(plan_data)) {
        total_crosses <- nrow(plan_data)
        assigned_crosses <- sum(plan_data$status == "Assigned")
        summary_data <- data.frame(
          Metric = c("Total Crosses", "Assigned Crosses", "Remaining Crosses", "Number of Cubicles", "Progress"),
          Value = c(
            total_crosses,
            assigned_crosses,
            total_crosses - assigned_crosses,
            length(cubicles()),
            paste0(round(assigned_crosses/total_crosses * 100, 1), "%")
          )
        )
        writeData(wb, "Summary", summary_data)
      }
      
      # Save the workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # Update download handler for optimized plan
  output$download_optimized_plan <- downloadHandler(
    filename = function() {
      paste("optimized_crossing_plan_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(rv$optimization_result)
      crosses <- rv$optimization_result$crosses
      
      if (!is.null(crosses) && nrow(crosses) > 0) {
        # Join with previous crosses if available
        if (!is.null(rv$previous_crosses)) {
          crosses <- crosses %>%
            left_join(rv$previous_crosses, 
                     by = c("Female.Parent", "Male.Parent"))
        }
        
        # Add rank column
        crosses <- crosses %>%
          mutate(Rank = row_number())
        
        writexl::write_xlsx(crosses, path = file)
      } else {
        dummy_data <- data.frame(Message = "No optimized crosses available. Please run the optimization first.")
        writexl::write_xlsx(dummy_data, path = file)
      }
    }
  )

  # Handle ratio displays
  output$cubicle_ratios <- renderUI({
    current_cubicles <- cubicles()
    if (length(current_cubicles) == 0) return(NULL)
    
    lapply(current_cubicles, function(cubicle) {
      female_count <- length(cubicle$crosses$Female.Parent)
      ratio <- female_count / 1  # 1 male
      
      ratio_color <- if (ratio > 3) "red" else "black"
      
      div(
        style = "margin-bottom: 10px;",
        p(
          strong("Cubicle ", cubicle$id, ": "),
          span(
            style = paste0("color: ", ratio_color, ";"),
            sprintf("Female to Male Ratio: %.1f:1", ratio)
          )
        )
      )
    })
  })

  # Handle print button
  observeEvent(input$print_layout, {
    layout_html <- div(
      h2("Breeding Cubicle Layout"),
      p("Generated on: ", format(Sys.time(), "%B %d, %Y")),
      hr(),
      lapply(cubicles(), function(cubicle) {
        div(
          style = "border: 1px solid black; padding: 10px; margin: 10px 0;",
          h4(paste("Cubicle", cubicle$id)),
          p(strong("Male: "), cubicle$male),
          p(strong("Females: "), paste(cubicle$crosses$Female.Parent, collapse = ", ")),
          p(strong("Notes: "), cubicle$notes)
        )
      })
    )
    
    showModal(modalDialog(
      layout_html,
      footer = tagList(
        actionButton("print_now", "Print"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })

  observeEvent(input$print_now, {
    runjs("window.print();")
  })

  # Enhanced statistics output
  output$statistics <- renderText({
    plan_data <- crossing_plan()
    if (is.null(plan_data)) return("No crossing plan loaded")
    
    total_crosses <- nrow(plan_data)
    assigned_crosses <- sum(plan_data$status == "Assigned")
    remaining_crosses <- total_crosses - assigned_crosses
    
    # Get counts for assigned males and females
    current_cubicles <- cubicles()
    
    # Count occurrences of males and females
    male_counts <- table(sapply(current_cubicles, function(x) x$male))
    female_counts <- table(unlist(sapply(current_cubicles, function(x) x$crosses$Female.Parent)))
    
    # Format the output
    male_text <- paste(names(male_counts), "-", male_counts, collapse = "\n")
    female_text <- paste(names(female_counts), "-", female_counts, collapse = "\n")
    
    paste0(
      "Total Crosses in Plan: ", total_crosses, "\n",
      "Assigned to Cubicles: ", assigned_crosses, "\n",
      "Remaining to Assign: ", remaining_crosses, "\n",
      "Number of Cubicles: ", length(cubicles()), "\n",
      "Progress: ", round(assigned_crosses/total_crosses * 100, 1), "%\n\n",
      "Male Usage:\n", male_text, "\n\n",
      "Female Usage:\n", female_text
    )
  })

  # Parent summary table
  output$parent_summary_table <- renderDT({
    current_cubicles <- cubicles()
    
    if (length(current_cubicles) == 0) {
      return(NULL)
    }
    
    # Count occurrences
    male_counts <- table(sapply(current_cubicles, function(x) x$male))
    female_counts <- table(unlist(sapply(current_cubicles, function(x) x$crosses$Female.Parent)))
    
    # Create summary tables
    male_summary <- data.frame(
      Parent = names(male_counts),
      Count = as.numeric(male_counts),
      Type = "Male",
      stringsAsFactors = FALSE
    )
    
    female_summary <- data.frame(
      Parent = names(female_counts),
      Count = as.numeric(female_counts),
      Type = "Female",
      stringsAsFactors = FALSE
    )
    
    # Combine summaries
    summary_df <- rbind(male_summary, female_summary)
    
    datatable(
      summary_df,
      options = list(
        pageLength = 15,
        dom = 't',
        ordering = TRUE
      ),
      rownames = FALSE
    )
  })
}

# Run the Shiny app
shinyApp(ui, server) 
  