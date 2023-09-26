
# INIT -----------------

## LIBRARIES -----------

library(brapi)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(brapirv1)
library(DT)
library(rjson)
library(reshape2)
library(AGHmatrix)
library(heatmaply)
library(shinyWidgets)
library(data.table)


## CUSTOM DATA and FUNCTION LOAD -----------

source("fxns.R")
source("demotrials_configs.R")





## INIT DB CONNECTION ----------------------
  
brap <- brapi::as.ba_db(
  secure = FALSE, 
  protocol = "https://", 
  db = "sugarcanebase.breedinginsight.net",
  port = 80, 
  apipath = NULL,
  multicrop = FALSE, 
  crop = "",
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD"), 
  token = "",
  granttype = "password",
  clientid = "rbrapi", 
  bms = FALSE, 
  version = "v1"
)

brap2 <- brapi::as.ba_db(
  secure = FALSE, 
  protocol = "https://", 
  db = "sugarcanebase.breedinginsight.net",
  port = 80, 
  apipath = NULL,
  multicrop = FALSE, 
  crop = "",
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD"), 
  token = "",
  granttype = "password",
  clientid = "rbrapi", 
  bms = FALSE, 
  version = "v2"
)

## CHECK if true connection
brapi::ba_check(brap) # true


# USER INTERFACE  -------------------------------------------------------------

ui <- dashboardPage(
  
  ## HEADER --------
   dashboardHeader(title = "STract"),
 
  ## SIDEBAR ------
   dashboardSidebar(
    
    dateInput("date", 
              "Choose A Date:"),
    
    actionButton("brapipull",
                 "Get Flower Inventory Data"), 
    
    
    sidebarMenu(
      menuItem("Home", 
               tabName="home",
               icon=icon("home")),
      
      menuItem("Flowering", 
               tabName = "flowering", 
               icon = icon("seedling")),
      
      menuItem("Kinship", 
               tabName = "kinship", 
               icon = icon("xmark")),
      
      menuItem("Performance", 
               tabName = "performance",
               icon = icon("star")),
      
      menuItem("Crosses",
               tabName = "crosses",
               icon = icon("envelope"))
    )
  ),
  
  ## BODY -----
  
  dashboardBody(
    tabItems(
      
      ### Home content ----
      tabItem(
        tabName="home",
        h1("USDA Sugarcane Crossing Tool"),
        
         p("Welcome to STract, the Sugarcane crossing tool! This tool lets you 
           see data and generate reports for all of the 
          clones that are flowering on a specific day.") ,
        
        h4("Here are instructions on how to use this tool."), 
        
        p("1. Choose a date from the calendar on the 
          left for the day you want to see"), 
        
        p("2. Click 'Get Flower Inventory Data' to pull in data from", 
          a(href = "https://sugarcanebase.breedinginsight.net/", "SugarcaneBase"),
          ". Thiswill show you an inventory list of flowering clones in the 'Flowering' tab to the left"),
        
        p("3. Next, you can click on any of the other tabs to see their associated data"),
        
        h4("Note, this tool is currently pointing to: "),
        
        textOutput("inventoryPointer")
        
      ),
      
      ### Flowering tab content -----
      tabItem(
        tabName = "flowering",
        fluidRow(
          box(p("The table to the right is a count of each clone that is
                flowering on the date you selected"), width=4),
          
          box(DTOutput("inventoryTable")))
      ),

      ### Pedigree tab content ----
      tabItem(
        tabName = "kinship",
        fluidRow(
          column(width=5,
          box(actionButton(inputId = "makepedigree",
                           label = "Get Pedigree Data"),
              p("The table to the right is the pedigree of each clone 
                that is flowering on the date you selected"),
              width=NULL),
          
          box(DTOutput("pedigreeTable"), 
              width=NULL)
          
          ),
        
          column(width=7,
          box(plotlyOutput("pedigreeMatrix"), 
              width=NULL)
      )
    )
  ),

      ### Performance tab content ----
      tabItem(
        tabName = "performance",
        fluidRow(
        column(width=5,
         box(actionButton(inputId = "makeperformance", 
                          label = "Get Performance Data"),
            p("The table to the right is the peformance of each clone 
              that is flowering on the date you selected"),
         
          #stuff for what phenotype to select
          uiOutput(outputId="colSelect"), #render html list output
          actionButton("selectCol","View Selected Data"))) 
        , 
        
        box(DTOutput("performanceTable")))
      ),


      ### Crosses tab content ----
      tabItem(
        tabName = "crosses",
        fluidRow(
          column(width=5,
          box(actionButton(inputId = "makecrosses",
                           label = "Get Cross Data"),
              p("The table to the right is a count of crosses that have been made with the clones that are flowering on the 
                date you selected"))),
          
          box(DTOutput("crossesTable")))
        
        
      )
    )
  )
)




# SERVER ---------------------------------------

server <- function(input, output) { 
  
  #choose date 
  reactive_date <- reactive({
    input$date
  })
  
  #point to current study
  output$inventoryPointer<-renderText(unique(brapi::ba_studies_table(con = brap, studyDbId = studydbid)$studyName))

  
  
## Flowering  ------
  
  inventory_init <- eventReactive(input$brapipull, withProgress(message="Pulling Inventory Data", {{
    inven<<-data.frame(brapi::ba_studies_table(con = brap, studyDbId = studydbid)) %>% 
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
  
  
## Pedigree and Progeny ----
  
  pedigree_init<- eventReactive(input$makepedigree, withProgress(message="Pulling Progeny Data", {{
    
    germplasm<<-as.data.frame(inventory_init())
    #germplasm<-inven #for testing 
    
    # pedigree<-as.data.frame(matrix(NA, ncol=4, nrow=dim(germplasm)[1]))
    # colnames(pedigree)<-c("GermplasmName", "GID", "Pedigree", "Progeny")
    # for(i in 1:dim(germplasm[1:5,])[1]){ #limited to first 20 for testing
    #   tmp<-fromJSON(brapi::ba_germplasm_details(con=brap, germplasmDbId=as.character(germplasm[i,2]), rclass="json"))
    #    pedigree[i,1]<-tmp$result$germplasmName
    #    pedigree[i,2]<-tmp$result$germplasmDbId
    #    pedigree[i,3]<-tmp$result$pedigree
    # }
    
    
    tmp<-ba_germplasm_details2(con=brap2, germplasmQuery=as.character(paste0("?studyDbId=",studydbid,"&pageSize=1000")), rclass="data.frame")
     #hacked ba_germplasm_details2 to get details for the entire study, then filtered with code below
    #oh, could have used ba_germplasm_details_study.... oh well
    
    pedigree<-tmp[tmp$data.germplasmName%in%germplasm$germplasmName,c("data.germplasmName", "data.germplasmDbId","data.pedigree")]
    
    
    for(i in 1:dim(pedigree)[1]){ 
      pedigree[i, 4]<-
        fromJSON(brapi::ba_germplasm_progeny(con=brap, germplasmDbId=as.character(pedigree[i,2]), rclass="json"))$metadata$pagination$totalCount
    }
    colnames(pedigree)[4]<-"N.progeny"

    pedigree[,c(1,3,4)]
    
  }}))
  
  pedmatrix_init<-eventReactive(input$makepedigree, {{
    germplasm<<-as.data.frame(inventory_init())
    
    mat<-PEDMATRIX(pedigree_download)
    
    if("LCP85-0384"%in%germplasm$germplasmName){
      axis<-germplasm$germplasmName
    } else {
      axis<- c(germplasm$germplasmName,"LCP85-0384" )
    }
    
    mat2<-round(mat[axis, axis],2) #subset
    
    mat2<-as.data.frame(mat2) #format for downstream
    mat2$data.germplasmName<-rownames(mat2) #add rownames 
    
    return(mat2)
  }})
  
  output$pedigreeTable<-({
    
    renderDT(merge(pedigree_init(), pedmatrix_init()[,c("LCP85-0384", "data.germplasmName")],
                   by="data.germplasmName"))}) #this merge fufills user request to see similarity to LCP85-0384
  
  output$pedigreeMatrix<-({renderPlotly(heatmaply(pedmatrix_init()))})
  
## Performance ----
  performance_init<- eventReactive(input$makeperformance, withProgress(message="Pulling Performance Data", {{

    germplasm<<-as.data.frame(inventory_init())

    tmp<-list()
    for(i in 1:dim(germplasm)[1]){
      tmp[[i]]<-brapi::ba_phenotypes_search(con=brap, germplasmDbId=as.character(germplasm[i,2]), rclass="data.frame", observationLevel="plot", pageSize = 20000)
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
    
    v<-aggregate(as.numeric(observations.value)~observations.observationVariableName+germplasmName, mean, data=tmp2, na.action=na.omit)
    colnames(v)[3]<-"observations.value"
    v$observations.value<-round(as.numeric(v$observations.value),2)
    v$observations.observationVariableName<-paste0(v$observations.observationVariableName, " mean")

    w<-aggregate(observations.value~observations.observationVariableName+germplasmName, sd, data=tmp2, na.action=na.omit)
    w$observations.value<-round(as.numeric(w$observations.value),2)
    w$observations.observationVariableName<-paste0(w$observations.observationVariableName, " sd")

    x<-aggregate(observations.value~observations.observationVariableName+germplasmName, length, data=tmp2, na.action=na.omit)
    x$observations.value<-round(as.numeric(x$observations.value),2)
    x$observations.observationVariableName<-paste0(x$observations.observationVariableName, " count" )

    z<-rbind(v,w,x)

    dcast(z, germplasmName~observations.observationVariableName)

    #get rid of crossing carts row and tassel count column

  }}))

  output$colSelect<-renderUI({
    pickerInput(inputId = 'phenoPick',
                label = 'Choose phenotypes to view',
                choices = colnames(performance_init()),
                options = list(`actions-box` = TRUE),multiple = T)
  })

  datasetInput <- eventReactive(input$selectCol,{

    datasetInput <- performance_init() %>%
      select(input$phenoPick)

    return(datasetInput)

  })

  output$performanceTable<-({renderDT(datasetInput(), extensions = "FixedColumns", options = list(
    scrollX = TRUE, fixedColumns = list(leftColumns = 2)))})

## Crosses ----
  
  #still need way to pull in new crosses? <- leave off here:
  
crosses_init<- eventReactive(input$makecrosses, withProgress(message="Pulling Cross Data", {{


  germplasm<-as.data.frame(inventory_init())

  freq_crosses<-as.data.frame(table(historical_crosses$Female.Parent, historical_crosses$Male.Parent))
  freq_crosses<-freq_crosses[-which(freq_crosses$Freq==0),] #get rid of crosses never made
  colnames(freq_crosses)<-c("Female.Parent", "Male.Parent", "Number.of.Crosses")

  progeny_crosses<-as.data.frame(aggregate(Number.of.Progenies~Female.Parent+Male.Parent,FUN=sum, data=historical_crosses))
  new_dataset <- freq_crosses %>% right_join(progeny_crosses, by=c("Female.Parent","Male.Parent"))

  ##change to AND for production!!!!!
  new_dataset2<-new_dataset[which(new_dataset$Female.Parent%in%germplasm$germplasmName&
                                    new_dataset$Male.Parent%in%germplasm$germplasmName),] #do or for testing....

  new_dataset2
}}))

  output$crossesTable<-({renderDT(crosses_init(), extensions = "FixedColumns", options = list(
    scrollX = TRUE, fixedColumns = list(leftColumns = 3)))})
  
  
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
