########INIT  ##########

########LIBRARIES##########
library(brapi)
library(tidyverse)
library(shiny)
library(shinydashboard)


########INIT DB CONNECTION ##########
# create connection object
#should update with a generic login!! 

brap <- brapi::as.ba_db(
  secure = FALSE, protocol = "https://", db = "sugarcanebase.breedinginsight.net",
  port = 80, apipath = NULL, multicrop = FALSE, crop = "",
  user = "keocorak", password = "AmbroseBenny2018", token = "", granttype = "password",
  clientid = "rbrapi", bms = FALSE, version = "v1"
)

#### Check if true connection
brapi::ba_check(brap) # true


###### USER INTERFACE  ############

ui <- dashboardPage(
  dashboardHeader(title = "STract"),
  dashboardSidebar(
    
    dateInput("date", "Choose A Date:"),
    
    actionButton("brapipull", "Get Data"), 
    
    sidebarMenu(
      menuItem("Home", tabName="home", icon=icon("home")),
      menuItem("Flowering", tabName = "flowering", icon = icon("seedling")),
      menuItem("Progeny", tabName = "progeny", icon = icon("child")),
      menuItem("Kinship", tabName = "kinship", icon = icon("xmark")),
      menuItem("Performance", tabName = "performance", icon = icon("star")),
      menuItem("Inventory", tabName = "inventory", icon = icon("envelope"))
    )
  ),
  dashboardBody(
    tabItems(
      
      #Home content
      tabItem(
        tabName="home",
        p(
          "Welcome to STract, the Sugarcane crossing tool! Choose a date from the calendar on the left.
          Then click 'Get Data' to pull in data from", a(href = "https://sugarcanebase.breedinginsight.net/", "SugarcaneBase"), ".")
      ),
      
      # First tab content
      tabItem(
        tabName = "flowering",
        fluidRow(
          box(p("The table to the right is a count of each clone that is flowering on the date you selected"), width=4),
          box(dataTableOutput("inventoryTable")))
      ),

      # Second tab content
      tabItem(
        tabName = "progeny",
        h2("Count of progeny of each parent"),
        h2("Matrix-Number of crosses made for each combination")
      ),

      # Third tab content
      tabItem(
        tabName = "kinship",
        h2("Pedigree for each Parent"),
        h2("Matrix of relatedness")
      ),

      # Fourth tab content
      tabItem(
        tabName = "performance",
        h2("Table of Parent Performance")
      ),


      # Fifth tab content
      tabItem(
        tabName = "inventory",
        h2("Table of available seedlots")
      )
    )
  )
)


###### SERVER ###########
server <- function(input, output) { 
  
  reactive_date <- reactive({
    input$date
  })

  inventory_init <- eventReactive(input$brapipull, {
    as.data.frame(brapi::ba_studies_table(con = brap, studyDbId = "3654")) %>% 
      filter(observationLevel=="plant") %>% #select just plant rows
      ## add row to filter on reactive data!
      count(germplasmName) #select just germplasm Name
    
      
  })

  output$inventoryTable<-({renderDataTable(inventory_init())})


  }

shinyApp(ui, server)

# # Define UI for application that draws a histogram
# ui <- fluidPage(
#
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
#
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
#
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
#
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
#
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }
#
# # Run the application
# shinyApp(ui = ui, server = server)
