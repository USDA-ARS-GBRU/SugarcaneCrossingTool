########INIT  ##########

########LIBRARIES##########
library(brapi)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(brapirv1)
library(DT)
library(rjson)
library(reshape2)

########INIT DB CONNECTION ##########
# create connection object
#should update with a generic login!! 

brap <- brapi::as.ba_db(
  secure = FALSE, protocol = "https://", db = "sugarcanebase.breedinginsight.net",
  port = 80, apipath = NULL, multicrop = FALSE, crop = "",
  user = Sys.getenv("USER"), password = Sys.getenv("PASSWORD"), token = "", granttype = "password",
  clientid = "rbrapi", bms = FALSE, version = "v1"
)

#### Check if true connection
brapi::ba_check(brap) # true

###### USER INTERFACE  ############

ui <- dashboardPage(
  dashboardHeader(title = "STract"),
  dashboardSidebar(
    
    dateInput("date", "Choose A Date:"),
    
    actionButton("brapipull", "Get Flower Inventory Data"), 
    
  
    
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
        h1("USDA Sugarcane Crossing Tool"),
         p(
          "Welcome to STract, the Sugarcane crossing tool! This tool lets you see data and generate reports for all of the 
          clones that are flowering on a specific day.") ,
        
        h4("Here are instructions on how to use this tool."), 
        
        p("1. Choose a date from the calendar on the left for the day you want to see"), 
        
        p("2. Click 'Get Flower Inventory Data' to pull in data from", a(href = "https://sugarcanebase.breedinginsight.net/", "SugarcaneBase"), ". This
          will show you an inventory list of flowering clones in the 'Flowering' tab to the left"),
        
        p("3. Next, you can click on any of the other tabs to see their associated data"),
        
        p("Note, this tool is currently pointing to: "),
        
        textOutput("inventoryPointer")
        
      ),
      
      # First tab content
      tabItem(
        tabName = "flowering",
        fluidRow(
          box(p("The table to the right is a count of each clone that is flowering on the date you selected"), width=4),
          box(DTOutput("inventoryTable")))
      ),

      # Second tab content
      tabItem(
        tabName = "kinship",
        fluidRow(
          box(actionButton(inputId = "makepedigree", label = "Get Pedigree Data"),
              p("The table to the right is the pedigree of each clone that is flowering on the date you selected"), width=4),
          box(DTOutput("pedigreeTable")))
      ),

      # Third tab content
      tabItem(
        tabName = "progeny",
        h2("Pedigree for each Parent"),
        h2("Matrix of relatedness")
      ),

      # Fourth tab content
      tabItem(
        tabName = "performance",
        fluidRow(
         box(actionButton(inputId = "makeperformance", label = "Get Performance Data"),
            p("The table to the right is the peformance of each clone that is flowering on the date you selected"), width=4),
        box(DTOutput("performanceTable")))
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
  
  output$inventoryPointer<-renderText(unique(brapi::ba_studies_table(con = brap, studyDbId = "3654")$studyName))

  
  
#########INVENTORY
  
  inventory_init <- eventReactive(input$brapipull, withProgress(message="Pulling Inventory Data", {{
    inven<<-data.frame(brapi::ba_studies_table(con = brap, studyDbId = "3654")) %>% 
      filter(observationLevel=="plant") %>% #select just plant rows
      separate(col = "Tassel.Count", into = c("Tassel.Count", "Timestamp"), sep = ",", remove = FALSE) %>%
      separate(col = "Timestamp", into = c("Timestamp", NA), sep = " ", remove = TRUE) %>% 
      filter(Timestamp == reactive_date()) %>% 
      select(germplasmName, germplasmDbId) %>% 
      group_by(germplasmName, germplasmDbId) %>% 
      summarise(count=n()) %>%  add_column(numberUsed=0) #select just germplasm Name
    
      
  }}))

  output$inventoryTable<-({renderDT(inventory_init(), options=list(language = list(
    zeroRecords = "There are no records to display. Double check the date you selected and try again. 
    You may need to wait a few minutes if inventory records were recently uploaded"))  , editable=list(target="column", disable=list(columns=c(0:2))))})
  
  
  #########Pedigree and Progeny
  
  pedigree_init<- eventReactive(input$makepedigree, withProgress(message="Pulling Progeny Data", {{
    
    germplasm<<-as.data.frame(inventory_init())
  
    pedigree<-as.data.frame(matrix(NA, ncol=4, nrow=dim(germplasm)[1]))
    colnames(pedigree)<-c("GermplasmName", "GID", "Pedigree", "Progeny")
    for(i in 1:dim(germplasm[1:5,])[1]){ #limited to first 20 for testing
      tmp<-fromJSON(brapi::ba_germplasm_details(con=brap, germplasmDbId=as.character(germplasm[i,2]), rclass="json"))
       pedigree[i,1]<-tmp$result$germplasmName
       pedigree[i,2]<-tmp$result$germplasmDbId
       pedigree[i,3]<-tmp$result$pedigree
    }
    
    for(i in 1:dim(germplasm[1:5,])[1]){ #limited to first 20 for testing
      pedigree[i, 4]<-
        fromJSON(brapi::ba_germplasm_progeny(con=brap, germplasmDbId=as.character(germplasm[i,2]), rclass="json"))$metadata$pagination$totalCount
    }
    
    pedigree
    
  }}))
  
  output$pedigreeTable<-({renderDT(pedigree_init())})
  
  
  #########Performance
  performance_init<- eventReactive(input$makeperformance, withProgress(message="Pulling Performance Data", {{
    
    germplasm<<-as.data.frame(inventory_init())
    
    tmp<-list()
    for(i in 1:dim(germplasm)[1]){
      tmp[[i]]<-brapi::ba_phenotypes_search(con=brap, germplasmDbId=as.character(germplasm[i,2]), rclass="data.frame", observationLevel="plot")
    }

    tmp2<-list()
    j=1
    for(i in 1:length(tmp)){
      if(length(dim(tmp[[i]]))>0){
        tmp2[[j]]<-as.data.frame(tmp[[i]])
        j=j+1
      }
      
      
    }
    
    tmp2<-bind_rows(tmp2)
    tmp2$observations.value<-as.numeric(tmp2$observations.value)
    v<-aggregate(observations.value~observations.observationVariableName+germplasmName, mean, data=tmp2, na.action=na.omit)
    dcast(v, germplasmName~observations.observationVariableName)
   
    #get rid of crossing carts row and tassel count column

  }}))
  
  output$performanceTable<-({renderDT(performance_init(), extensions = "FixedColumns", options = list(
    scrollX = TRUE, fixedColumns = list(leftColumns = 2)))})
  

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
