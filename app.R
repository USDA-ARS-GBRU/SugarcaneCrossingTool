# app.R

# INIT -----------------

## LIBRARIES -----------
# Load required libraries
library(plyr)
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
      "Step 3: Choose A Date"
    ),
    #actionButton("brapipull", "Get Flower Inventory Data"),
    textOutput("dateWarning"), 
    

    
    actionButton(
      "brapipull",
     strong("Step 4. Get Flower Inventory")
    ),
    p("Don't forget to push 'Get Flower Inventory", strong("each"), "each time you choose a new date"),
    sidebarMenu(
      menuItem("Home",
               tabName = "home",
               icon = icon("home")
      ),
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
    tabItems(

      ### Home content ----
      tabItem(
        tabName = "home",
        
        h1("Sugarcane Integrated Breeding System (SIBS) Sugarcane Crossing Tool (SCT)"),
        p("Welcome to SCT! To use this app, follow the instructions below:"),
        p("* Start by loggin in: from the sidebar on the left, (1) select a location and (2) breeder name."),
        p("* Then, chose an (3) inventory date."),
        p("* Next, (4) Click on the 'Get Flower Inventory' button"),
        p("* After that, move to the Flowering Inventory tab to view your inventory data"),
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
              # column(
              #   width = 4,
              #   h4("Available Clones"),
              #   uiOutput("available_clones")
              # ),
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

        
        # fluidRow(
        # 
        #   box(
        #     title = "Inventory",
        #     p("These tables show the number and location of male and female clones flowering today"),
        #     width = 12,
        #     fluidRow(
        #       column(
        #         width = 4,
        #         h4("Male Clones"),
        #         DTOutput("inventoryTableMale")
        #       ),
        #       column(
        #         width = 4,
        #         h4("Female Clones"),
        #         DTOutput("inventoryTableFemale")
        #       
        #       )
        #     )
        #   )
        # ), 
        # 

        fluidRow(
        box(title="These tables shows you all parents flowering today, sorted into male and female columns based on techician's inventory",
             textOutput("dataSourceText"),
            width=12,
            fluidRow(
              box(column(
                width=4,
                DTOutput("inventoryTableMale")
              )),
             box(column(
                width=4,
                DTOutput("inventoryTableFemale")
              ))
            ))),

       


        
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
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Previous Crosses",
        fluidRow(
          box(
            title="Previous crosses made with selected clones",
            width=12,
            actionButton(
              inputId = "makecrosses",
              label = "Get Data on Previous Crosses and Seedlots"
            ),
            p("This table shows a count of", strong("previous"), "crosses made with your selected clones. It also shows the total number of progeny and availablity of seedlots for these crosses. This table is filtered based on categorizatons made in step 5.
              If the cross was made earlier this year, the 'Progeny.Per.Cross' column will read 'None yet, new cross this year'.")
          )
        ),
        DTOutput("crossesTable")
      ),
      tabPanel(
        "Previous Reciprocal Crosses",
        fluidRow(
          box(
            title="Previous RECIPROCAL crosses made with selected clones",
            width=12,
            actionButton(
              inputId = "makerecipcrosses",
              label = "Get Data on Previous RECIPROCAL Crosses and Seedlots"
            ),
            p("This table shows a count of", strong("previous RECIPROCAL"), "crosses made with your selected clones. It also shows the total number of progeny and availablity of seedlots for these crosses. This table is filtered based on categorizatons made in step 5.
              If the cross was made earlier this year, the 'Progeny.Per.Cross' column will read 'None yet, new cross this year'.")
          )
        ), 
        DTOutput("recipCrossesTable")
      )
      )
      )
      ,

  

      
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
        p("BETA implementation of", a(href="https://github.com/Resende-Lab/SimpleMating", "SimpleMating R package"), "This optimizes the midparent value of potential cross combinations based on a weighted selection index of parental BVs. Breeding values were predicted from S4 trial data and a pedigree relationship matrix. Potential crosses are culled based on pairwise-K value (where value of K is proportional to relatedness."),
        fluidRow(
          box(
            title = "Optimization Parameters",
            numericInput("n_crosses", "Number of Crosses to Select:", 10, min = 1, max = 100),
            numericInput("max_crosses_per_parent", "Max Crosses per Parent:", 3, min = 1, max = 10),
            #numericInput("min_crosses_per_parent", "Min Crosses per Parent:", 1, min = 0, max = 5),
            sliderInput("culling_k", "Culling Pairwise K:", 
                       min = 0, max = 1, value = 1, step = 0.05),
            
            p(strong("Trait Weights:")),
            textOutput("weight_sum_warning"),
            numericInput(inputId="brix", label="Average Brix", value = round(1/3,1)),
            numericInput(inputId="biomass", label="Total Biomass", value = round(1/3,1)),
            numericInput(inputId="ratoon", label="Ratooning Ability", value = round(1/3,1)),
            
            
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
            plotlyOutput("optimization_plot")
          )
        )
      )
    )
  )
)

inventory_init <<- eventReactive(input$brapipull, withProgress(message = "Pulling Inventory Data", {
  tryCatch({
    
    inven <- data.frame(brapi::ba_studies_table(con = brap, studyDbId = reactive_iid(), rclass="data.frame")) %>%
      filter(observationLevel == "plot") %>% # select just plant rows
      set_names(~(.)%>% str_replace_all("SUGARCANE.*","") %>% str_replace_all("\\.","")) %>% # take CO term out of colnames
      mutate_at('blockNumber', as.factor)
    
    inven$blockNumber<-revalue(inven$blockNumber, c("1"="West", "2"="East", "3"="Railcarts", "4"="Back"))
    
    inven_male<-filter(inven, grepl(reactive_date(),TasselCountMale)) %>% 
      select(germplasmName, blockNumber, notes, TasselCountMale) %>% 
      separate(TasselCountMale, into=c("Count",NA), sep=",") %>%
      group_by(germplasmName)
    
    male<-merge(aggregate(as.numeric(Count)~germplasmName,inven_male, sum ),
                aggregate(blockNumber~germplasmName,inven_male, function(x) paste(unique(x), collapse=":")))
    
    inven_female<-filter(inven, grepl(reactive_date(),TasselCountFemale)) %>% 
      select(germplasmName, blockNumber, notes, TasselCountFemale) %>% 
      separate(TasselCountFemale, into=c("Count",NA), sep=",") %>%
      group_by(germplasmName)
    
    female<-merge(aggregate(as.numeric(Count)~germplasmName,inven_female, sum ),
                  aggregate(blockNumber~germplasmName,inven_female, function(x) paste0(unique(x), collapse=":")))
    
    colnames(male)<-colnames(female)<-c("Clone", "FlowerCount", "Location")
    
    inven2<-list(male, female)
    names(inven2)<-c("male", "female")
    
    dataSource("Data pulled from BrAPI")
    inven2
  }, error = function(e) {
    dataSource("Saved data is being rendered")
    data.frame(Clone = character(), FloweringCount = numeric(), Location = character()) # Return an empty data frame with the expected columns
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
    previous_crosses = NULL,
    recip_previous_crosses=NULL,
    selectedColumns = NULL
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
  #clone_assignments <- reactiveVal(list(available = character(), male = character(), female = character()))
  
clone_assignments <- reactiveVal(list(male = character(), female = character()))
  
  
  # Initialize available clones
  # observe({
  #   inventory_data <- inventory_init()
  #   clones <- unique(inventory_data$Clone)
  #   clone_assignments(list(available = clones, male = character(), female = character()))
  # })

observe({
  inventory_data <- inventory_init()
  males <- unique(inventory_data$female$Clone)
  females<-unique(inventory_data$female$Clone)
  clone_assignments(list(male = males, female =females))
})


  
  # Render sortable lists
  # output$available_clones <- renderUI({
  #   bucket_list(
  #     header = "Available Clones",
  #     group_name = "clone_buckets",
  #     orientation = "vertical",
  #     add_rank_list(
  #       text = "Drag clones from here",
  #       labels = clone_assignments()$available,
  #       input_id = "available_list"
  #     )
  #   )
  # })
  # 
  # output$male_parents <- renderUI({
  #   bucket_list(
  #     header = "Male Parents",
  #     group_name = "clone_buckets",
  #     orientation = "vertical",
  #     add_rank_list(
  #       text = "Drag male parents here",
  #       labels = clone_assignments()$male,
  #       input_id = "male_list"
  #     )
  #   )
  # })
  
  # output$female_parents <- renderUI({
  #   bucket_list(
  #     header = "Female Parents",
  #     group_name = "clone_buckets",
  #     orientation = "vertical",
  #     add_rank_list(
  #       text = "Drag female parents here",
  #       labels = clone_assignments()$female,
  #       input_id = "female_list"
  #     )
  #   )
  # })
  # 
  # Update clone assignments when lists change
  # observe({
  #   clone_assignments(list(
  #     available = input$available_list,
  #     male = input$male_list,
  #     female = input$female_list
  #   ))
  # })


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
    male = input$male_list,
    female = input$female_list
  ))
})

  



  # Cross Optimization
  observeEvent(input$run_optimization, {
    # Get current inventory data
    #inventory_data <- inventory_init()
    inventory_data <- as.data.frame(rbind(inventory_init()$male, inventory_init()$female))
    
    # Get selected parents
    male_parents <- as.data.frame(inventory_init()$male)$Clone
    female_parents <- as.data.frame(inventory_init()$female)$Clone
    
    ck<-input$culling_k
    
    # Check if parents are selected
    if (length(male_parents) == 0 || length(female_parents) == 0) {
      showNotification("Please select both male and female parents before running optimization.", type = "error")
      return()
    }
    
    # Run optimization
    optimized_crosses <- tryCatch({
      optimize_crosses(inventory_data, 
                       male_parents=male_parents,
                       female_parents=female_parents,
                       n_crosses = input$n_crosses,
                       #min_crosses_per_parent = 1,
                       max_crosses_per_parent = input$max_crosses_per_parent,
                       # min_crosses_per_parent = input$min_crosses_per_parent,
                       culling_k = input$culling_k,
                       #prop_sel = input$prop_sel,
                       blup=blup_data[,1:4],
                       amat=as.matrix(parent_amat),
                       weights=c(input$brix, input$biomass, input$ratoon))
    }, error = function(e) {
      showNotification(paste("Error in optimization:", e$message), type = "error")
      return(list(crosses = data.frame(), plot = NULL))
    })
    
    # Display results
    output$optimized_crosses_table <- renderDT({
      if (!is.null(optimized_crosses$crosses) && nrow(optimized_crosses$crosses) > 0) {
        # Join with previous crosses if available
        if (!is.null(rv$previous_crosses)) {
          optimized_crosses$crosses <- optimized_crosses$crosses %>%
            left_join(rv$previous_crosses, 
                     by = c("Female.Parent", "Male.Parent"))
        }
        
        datatable(optimized_crosses$crosses, 
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
    
     output$optimization_plot <- renderPlotly({
      if (!is.null(optimized_crosses$plot)) {
        # Extract plot data and ensure it has all required columns
        plot_data <- optimized_crosses$plot$data
        
        # Add rank column and selected status
        #plot_data$Rank <- 1:nrow(plot_data)
        #plot_data$Selected <- plot_data$Rank <= input$n_crosses
        
        # Create hover text based on available columns
        hover_text <- paste(
          #"Rank:", plot_data$Rank,
          "\nParent1:", plot_data$Parent1,
          "\nParent2:", plot_data$Parent2,
          "\nSelection Index:", round(plot_data$Y, 3),
          "\nKinship:", round(plot_data$K, 3)
        )
        
        # Add additional information if available
        if ("Seed.Quantity.grams" %in% names(plot_data)) {
          hover_text <- paste(hover_text, 
                            "\nSeed Quantity:", plot_data$Seed.Quantity)
        }
        if ("Previous.Cross.Names" %in% names(plot_data)) {
          hover_text <- paste(hover_text, 
                            "\nPrevious Crosses:", plot_data$Previous.Cross.Names)
        }
        
        # Create new ggplot with hover text and vertical line
        p <- ggplot(plot_data, aes(x = K, y = Y)) +
          # Color points based on selection status
          geom_point(aes(color = Sel), size = 3, alpha = 0.7) +
          #scale_color_manual(values = c("Non-Selected" = "gray", "Mating Plan" = "blue")) +
          geom_vline(xintercept = ck, linetype = "dashed", 
                    color = "red", size = 1) +
          theme_minimal() +
          theme(
            panel.grid.major = element_line(color = "gray90"),
            panel.grid.minor = element_line(color = "gray95"),
            axis.text = element_text(color = "gray30"),
            axis.title = element_text(color = "gray30", size = 12),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            legend.position = "top"
          ) +
          labs(
            x = "Kinship Coefficient",
            y = "Selection Index",
            title = paste("Cross Optimization Plot (Top", input$n_crosses, "Crosses Highlighted)"),
            color = "Selected Crosses"
          ) +
          aes(text = hover_text)
        
        ggplotly(p, tooltip = "text") %>%
          layout(
            hoverlabel = list(bgcolor = "white"),
            plot_bgcolor = "white",
            paper_bgcolor = "white"
          )
      } else {
        plot_ly() %>%
          add_annotations(
            text = "No plot available",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
      }
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
  
    # output$optimization_plot <- renderPlot({
    #   if (!is.null(optimized_crosses$plot)) {
    #     optimized_crosses$plot
    #   } else {
    #     plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
    #     text(0, 0, "No plot available", cex = 1.5)
    #   }
    # })

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
}

# Run the Shiny app
shinyApp(ui, server) 
  