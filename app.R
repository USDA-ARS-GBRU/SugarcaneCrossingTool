# app.R

# INIT -----------------

## LIBRARIES -----------
# Load required libraries

library(brapi)
library(tidyverse)
library(shiny)
library(bs4Dash)
library(brapirv1)
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
  title = "STracT",

  ## CONTROLBAR ----
  controlbar = dashboardControlbar(
    collapsed = TRUE,
    div(class = "p-3", skinSelector()),
    pinned = FALSE
  ),

  ## HEADER --------
  header = dashboardHeader(title = "Sugarcane Crossing Tool"),

  ## SIDEBAR ------
  sidebar = dashboardSidebar(
    selectInput("location", "Select Location:", choices = location_iid_map),
    textInput("crossesid", "Login with your CID:", value=""),
    
    dateInput(
      "date",
      "Choose A Date:",
      value = "2023-10-30"
    ),
    
    p("for testing, select:", strong("October 10, 2023")),
    
    actionButton(
      "brapipull",
      "Get Flower Inventory Data"
    ),
    p("Don't forget to push 'Get Flower Inventory Data'", strong("each"), "each time you choose a new date"),
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
      menuItem("Download Data",
               tabName = "download",
               icon = icon("download")
      )
    )
  ),

  ## BODY -----

  body = dashboardBody(
    tabItems(

      ### Home content ----
      tabItem(
        tabName = "home",
        
        h1("USDA Sugarcane Crossing Tool"),
        p("Welcome to STracT, the Sugarcane crossing tool! Click", a(href="https://github.com/keocorak/XingAppV2", "here"), "for instructions."),
        p("Placeholder for image"),
        
        p("Testing and debugging stuff below:"),
        p("Note, you've logged in to view inventory for these can lines: "),
        textOutput("inventoryPointer"),
        
        p("Note, you've logged in to track this crossing experiment: "),
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
              label = "Get Cross Data"
            ),
            p("This table shows a count of crosses that have been made with the clones that are flowering on the 
              date you selected as well as the summed number of progeny produced from those crosses. If the cross was made earlier this year, the 'Progeny.Per.Cross' column will read 'None yet, new cross this year'.")
          )
        ),
        DTOutput("crossesTable")
      ),

      #### Download tab content ----
      tabItem(
        tabName = "download",
        fluidRow(
          box(p("This button will allow you to download a full data report as an excel file.
                A partial download will fail, so make sure you've pulled all the inventory, pedigree, performance and cross data.
                A successful download will have a data in the file name."),
              downloadButton("downloaddata", "Download Data"))
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
  reactive_cid <- reactive({input$crossesid})
  
  # Add the renderText for dataSourceText
  output$dataSourceText <- renderText({
    dataSource()
  })
  
  # Call the server functions from separate files
  inventory_init <- flowering_server(input, output, session, reactive_date, reactive_iid, dataSource)
  pedigree_server(input, output, session, reactive_iid, selectedClone, inventory_init)
  performance_server(input, output, session, reactive_iid, rv, rv_trait_scatter, inventory_init)
  crosses_server(input, output, session, reactive_iid, reactive_cid)
  download_page_server(input, output, session, reactive_date)
  
  # Output for inventory pointer
  output$inventoryPointer <- renderText({
    location <- names(location_iid_map)[location_iid_map == input$location]
    paste("Location:", location, "-", unique(brapi::ba_studies_table(con = brap, studyDbId = input$location)$studyName))
  })

  # Output for cross pointer
  output$crossPointer <- renderText({
    validate(
      need(input$crossesid != "", "Please log in with your CID")
    )
    unique(ba_crosses_study(con = brap2, crossingProjectDbId = as.character(input$crossesid), rclass = "data.frame")$data.crossingProjectName[[1]])
  })
}

# Run the Shiny app
shinyApp(ui, server)