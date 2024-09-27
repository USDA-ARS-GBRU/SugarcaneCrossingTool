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
library(SimpleMating)
library(sortable)
library(shinyjs)


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
    selectInput("location", "Select Location:", choices = location_iid_map),
    
    #this is kind of confusing. The idea is that multiple breeders might be working at same location (Florida) and they should be able to track crosses independently, even though cane lines are combined
    #so crossesid refers to crosses a specific breeder is making
    selectInput("crossesid", "Select Breeder", choices=crosses_iid_map), 
    
    dateInput(
      "date",
      "Choose A Date:",
      value = "2023-10-10"
    ),
    
    p("for testing, select:", strong("October 10, 2023")),
    
    actionButton(
      "brapipull",
      "Get Flower Inventory Data"
    ),
    p("Don't forget to push 'Get Flower Inventory Data'", strong("each"), "time you choose a new date"),
    sidebarMenu(
      menuItem("Home",
               tabName = "home",
               icon = icon("home")
      ),
      menuItem("Flowering Inventory",
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
      menuItem("Cross Optimization",
               tabName = "optimization",
               icon = icon("dna")
      ),
      menuItem("Download Data",
               tabName = "download",
               icon = icon("download")
      )
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
      "))
    ),
    tabItems(

      ### Home content ----
      tabItem(
        tabName = "home",
        
        h1("Sugarcane Integrated Breeding System (SIBS) Sugarcane Crossing Tool (SCT)"),
        p("Welcome SCT! Click", a(href="https://github.com/USDA-ARS-GBRU/SugarcaneCrossingTool", "here"), "for instructions."),
       
        h1("Login information"),
        
        p("You've logged in to view inventory for this location: "),
        
        textOutput("inventoryPointer"),
        
        br(),
        
        p("You've logged in as:"),
        
        textOutput("crossPointer")
      ),

      ### Flowering tab content -----
      tabItem(
        tabName = "flowering",
        fluidRow(
          box(p("This table shows you the count and sex of each clone that is flowering on the day you selected.")),
          textOutput("dataSourceText"),
          DTOutput("inventoryTable")
        )
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
                actionButton(
                  inputId = "makepedigree",
                  label = "Get Pedigree Data"
                ),
                p("This table shows you the pedigree of each clone that is flowering on the date you selected, 
                  as well as the number of progeny it produced and its relatedness (0-1+) to LCP85-384.")
              ),
              DTOutput("pedigreeTable"),
            )
          ),
          tabPanel(
            "Relationship Matrix",
            box(p("This is a relationship matrix of the clones that are flowering on the date you selected. Values closer to one indicate high relatedness. You can zoom in to particular regions of the matrix.")),
            plotlyOutput("pedigreeMatrix")
          ),
          tabPanel(
            "Visualize Pedigrees",
            fluidRow(
              box(
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
                actionButton(
                  inputId = "makeperformance",
                  label = "Get Performance Data"
                ),
                p("This table shows the mean and sd of the performance for each clone 
                  that is flowering on the date you selected. Once data has been pulled from the database, you will be able to select traits to view using the drop-down menu. This step may take several minutes, please be patient."),
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
            actionButton(
              inputId = "makecrosses",
              label = "Get Data on Previous Crosses and Seedlots"
            ),
            p("This table shows a count of previous crosses that could be made with the clones that are flowering today and the summed number of progeny produced from those crosses. 
              It also shows you the availability of exisiting seedlots for the crosses that could be made today.
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
    )
  )
),

      ### Cross Optimization tab content ----
      tabItem(
        tabName = "optimization",
        fluidRow(
          box(
            title = "Select Parents",
            width = 12,
            fluidRow(
              column(
                width = 4,
                h4("Available Clones"),
                uiOutput("available_clones")
              ),
              column(
                width = 4,
                h4("Male Parents"),
                uiOutput("male_parents")
              ),
              column(
                width = 4,
                h4("Female Parents"),
                uiOutput("female_parents")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Optimization Parameters",
            numericInput("n_crosses", "Number of Crosses to Select:", 10, min = 1, max = 100),
            numericInput("max_crosses_per_parent", "Max Crosses per Parent:", 3, min = 1, max = 10),
            numericInput("min_crosses_per_parent", "Min Crosses per Parent:", 1, min = 0, max = 5),
            numericInput("culling_k", "Culling Pairwise K:", 1, min = 0, max = 2, step = 0.1),
            numericInput("prop_sel", "Proportion to Select:", 0.05, min = 0.01, max = 0.5, step = 0.01),
            actionButton("run_optimization", "Run Optimization")
          ),
          box(
            title = "Optimized Crossing Plan",
            DTOutput("optimized_crosses_table")
          )
        ),
        fluidRow(
          box(
            title = "Optimization Visualization",
            plotOutput("optimization_plot")
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
  
  # Add the renderText for dataSourceText
  output$dataSourceText <- renderText({
    dataSource()
  })
  
  # Call the server functions from separate files
  inventory_init <- flowering_server(input, output, session, reactive_date, reactive_iid, dataSource)
  pedigree_server(input, output, session, reactive_iid, selectedClone, inventory_init)
  performance_server(input, output, session, reactive_iid, rv, rv_trait_scatter, inventory_init)
  crosses_server(input, output, session, reactive_cid, inventory_init)
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
  
  # Cross Optimization
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
    optimized_crosses <- tryCatch({
      optimize_crosses(inventory_data, 
                       male_parents,
                       female_parents,
                       n_crosses = input$n_crosses,
                       max_crosses_per_parent = input$max_crosses_per_parent,
                       min_crosses_per_parent = input$min_crosses_per_parent,
                       culling_k = input$culling_k,
                       prop_sel = input$prop_sel)
    }, error = function(e) {
      showNotification(paste("Error in optimization:", e$message), type = "error")
      return(list(crosses = data.frame(), plot = NULL))
    })
    
    # Display results
    output$optimized_crosses_table <- renderDT({
      if (!is.null(optimized_crosses$crosses) && nrow(optimized_crosses$crosses) > 0) {
        datatable(optimized_crosses$crosses, options = list(pageLength = 10))
      } else {
        datatable(data.frame(Message = "No crosses found or error occurred"), options = list(pageLength = 10))
      }
    })
    
    output$optimization_plot <- renderPlot({
      if (!is.null(optimized_crosses$plot)) {
        optimized_crosses$plot
      } else {
        plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(0, 0, "No plot available", cex = 1.5)
      }
    })

    output$download_optimized_plan <- downloadHandler(
  filename = function() {
    paste("optimized_crossing_plan_", Sys.Date(), ".xlsx", sep = "")
  },
  content = function(file) {
    # Check if optimized crosses exist
    if (!is.null(optimized_crosses$crosses) && nrow(optimized_crosses$crosses) > 0) {
      writexl::write_xlsx(optimized_crosses$crosses, path = file)
    } else {
      # If no optimized crosses, create a dummy dataframe with a message
      dummy_data <- data.frame(Message = "No optimized crosses available. Please run the optimization first.")
      writexl::write_xlsx(dummy_data, path = file)
    }
  }
)
  })

  observeEvent(input$dark_mode, {
    shinyjs::toggleClass(selector = "body", class = "dark-mode")
  })
}

# Run the Shiny app
shinyApp(ui, server)
  