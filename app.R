# INIT -----------------

## LIBRARIES -----------

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


## CUSTOM DATA and FUNCTION LOAD -----------

source("fxns.R")
source("demotrials_configs.R")

## THEME


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
  title="STracT",
  
  ## CONTROLBAR ----
  controlbar = dashboardControlbar(
    collapsed = TRUE,
    div(class = "p-3", skinSelector()),
    pinned = FALSE
  ),

  
  ## HEADER --------
  header=dashboardHeader(title = "STracT"),

  ## SIDEBAR ------
  sidebar=dashboardSidebar(
    dateInput(
      "date",
      "Choose A Date:"
    ),
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
      menuItem("Possible Crosses",
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

  body=dashboardBody(
    
    
    
    
    tabItems(

      ### Home content ----
      tabItem(
        tabName = "home",
        h1("USDA Sugarcane Crossing Tool"),
        p("Welcome to STracT, the Sugarcane crossing tool! This tool lets you 
           see data and generate reports for all of the 
          clones that are flowering on a specific day."),
        h4("Here are instructions on how to use this tool."),
        p("1. Choose a date from the calendar on the 
          left for the day you want to see"),
        p(
          "2. Click 'Get Flower Inventory Data' to pull in data from",
          a(href = "https://sugarcanebase.breedinginsight.net/", "SugarcaneBase"),
          ". Thiswill show you an inventory list of flowering clones in the 'Flowering' tab to the left"
        ),
        p("3. Next, you can click on any of the other tabs to see their associated data"),
        h4("Note, this tool is inventoring these can lines: "),
        textOutput("inventoryPointer"),
        h4("Note, this tool is tracking this crossing experiment: "),
        textOutput("crossPointer")
      ),

      ### Flowering tab content -----
      tabItem(
        tabName = "flowering",
        fluidRow(
          box(p("This table shows you a count of each clone that is flowering on the day you selected.")),
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
                    as well as the number of progeny it produced and it's relatedness (0-1+) to LCP85-384.")

                ),
               DTOutput("pedigreeTable"),

              )
            ),
          tabPanel("Relationship Matrix", 
                   box(p("This is a relationshiop matrix of the clones that are flowering on the date you selected.")),
                   plotlyOutput("pedigreeMatrix"))
        )
      ),

      ### Performance tab content ----
      tabItem(
        tabName = "performance",
        fluidRow(
          column(
            width = 5,
            box(
              actionButton(
                inputId = "makeperformance",
                label = "Get Performance Data"
              ),
              p("The table to the right is the peformance of each clone 
              that is flowering on the date you selected"),

              # stuff for what phenotype to select
              uiOutput(outputId = "colSelect"), # render html list output
              actionButton("selectCol", "View Selected Data")
            )
          ),
          box(DTOutput("performanceTable"))
        )
      ),


      ### Crosses tab content ----
      tabItem(
        tabName = "crosses",
        fluidRow(
          column(
            width = 5,
            box(
              actionButton(
                inputId = "makecrosses",
                label = "Get Cross Data"
              ),
              p("The table to the right is a count of crosses that have been made with the clones that are flowering on the 
                date you selected")
            )
          ),
          box(DTOutput("crossesTable"))
        )
      ),

      #### Download tab content ----
      tabItem(
        tabName = "download",
        fluidRow(
          downloadButton("downloaddata", "Download Data")
        )
      )
    )
  )
)


# SERVER ---------------------------------------

server <- function(input, output) { 
  
  # choose date
 reactive_date <- reactive({
   input$date
 })
 
 #output$date<-renderText(reactive_date())

 # point to current study
 output$inventoryPointer <- renderText(unique(brapi::ba_studies_table(con = brap, studyDbId = studydbid)$studyName))

 output$crossPointer <- renderText(unique(ba_crosses_study(con = brap2, crossingProjectDbId = crossingprojectdbid, rclass = "data.frame")$data.crossingProjectName[[1]]))

## Flowering  ------
  
  inventory_init <- eventReactive(input$brapipull, withProgress(message = "Pulling Inventory Data", {{ inven <- data.frame(brapi::ba_studies_table(con = brap, studyDbId = studydbid)) %>%
  filter(observationLevel == "plant") %>% # select just plant rows
  separate(col = "Tassel.Count", into = c("Tassel.Count", "Timestamp"), sep = ",", remove = FALSE) %>%
  separate(col = "Timestamp", into = c("Timestamp", NA), sep = " ", remove = TRUE) %>%
  filter(Timestamp == reactive_date()) %>%
  select(germplasmName, germplasmDbId, Sex..M.F.WM) %>%
  group_by(germplasmName, germplasmDbId,Sex..M.F.WM ) %>%
  summarise(count = n()) %>%
  add_column(Number.Used = 0) %>% # select just germplasm Name
  rename(Clone = germplasmName, Flowering.Count = count, Sex=Sex..M.F.WM) }}))

  output$inventoryTable<-({renderDT(inventory_init()[,-which(colnames(inven)=="germplasmDbId")], options=list(language = list(
    zeroRecords = "There are no records to display. Double check the date you selected and try again. 
    You may need to wait a few minutes if inventory records were recently uploaded"))  , editable=list(target="column", disable=list(columns=c(0:2))))})
  
  
## Pedigree and Progeny ----
  
  pedigree_init <- eventReactive(input$makepedigree, withProgress(message = "Pulling Progeny Data", {{ germplasm <- as.data.frame(inventory_init())
  # germplasm<-inven #for testing


  tmp <- stripClass(
    as.data.frame(
      ba_germplasm_details2(con = brap2, germplasmQuery = as.character(paste0("?studyDbId=", studydbid, "&pageSize=1000")), rclass = "data.frame")
    ),
    classString = "ba_germplasm_details"
  )
  # hacked ba_germplasm_details2 to get details for the entire study, then filtered with code below
  # oh, could have used ba_germplasm_details_study.... oh well

  pedigree <- tmp[tmp$data.germplasmName %in% germplasm$Clone, c("data.germplasmName", "data.germplasmDbId", "data.pedigree")] %>%
    rename(Clone = data.germplasmName, Pedigree = data.pedigree)

  # this loop adds the number of progeny
  for (i in 1:dim(pedigree)[1]) {
    pedigree[i, 4] <-
      fromJSON(brapi::ba_germplasm_progeny(con = brap, germplasmDbId = as.character(pedigree[i, 2]), rclass = "json"))$metadata$pagination$totalCount
  }

  colnames(pedigree)[4] <- "Number.Progeny"

  pedigree[, -which(colnames(pedigree) == "data.germplasmDbId")] }}))

pedmatrix_init <- eventReactive(input$makepedigree, {{ germplasm <<- as.data.frame(inventory_init())

  mat <- PEDMATRIX(pedigree_download)

  if ("LCP85-0384" %in% germplasm$Clone) {
    axis <- germplasm$Clone
  } else {
    axis <- c(germplasm$Clone, "LCP85-0384")
  }

  mat2 <- round(mat[axis, axis], 2) # subset

  mat2 <- as.data.frame(mat2) # format for downstream
  mat2$Clone <- rownames(mat2) # add rownames

  return(mat2) }})

output$pedigreeTable <- ({
  renderDT(merge(pedigree_init(), pedmatrix_init()[, c("LCP85-0384", "Clone")],
    by = "Clone"
  ) %>% rename(Rel.2.LCP850384 = "LCP85-0384"))
}) # this merge fufills user request to see similarity to LCP85-0384

output$pedigreeMatrix <- ({
  renderPlotly(heatmaply(pedmatrix_init()))
})


## Performance ----
performance_init <- eventReactive(input$makeperformance, withProgress(message = "Pulling Performance Data", {{ germplasm <- as.data.frame(inventory_init())

  tmp <- list()
  for (i in 1:dim(germplasm)[1]) {
    tmp[[i]] <- brapi::ba_phenotypes_search(con = brap, germplasmDbId = as.character(germplasm[i, 2]), rclass = "data.frame", observationLevel = "plot", pageSize = 20000)
  }

  tmp2 <- list()
  j <- 1
  for (i in 1:length(tmp)) {
    if (length(dim(tmp[[i]])) > 0) {
      tmp2[[j]] <- as.data.frame(tmp[[i]])
      j <- j + 1
    }
  }

  tmp2 <- bind_rows(tmp2)

  v <- aggregate(as.numeric(observations.value) ~ observations.observationVariableName + germplasmName, mean, data = tmp2, na.action = na.omit)
  colnames(v)[3] <- "observations.value"
  v$observations.value <- round(as.numeric(v$observations.value), 2)
  v$observations.observationVariableName <- paste0(v$observations.observationVariableName, " mean")

  w <- aggregate(observations.value ~ observations.observationVariableName + germplasmName, sd, data = tmp2, na.action = na.omit)
  w$observations.value <- round(as.numeric(w$observations.value), 2)
  w$observations.observationVariableName <- paste0(w$observations.observationVariableName, " sd")

  x <- aggregate(observations.value ~ observations.observationVariableName + germplasmName, length, data = tmp2, na.action = na.omit)
  x$observations.value <- round(as.numeric(x$observations.value), 2)
  x$observations.observationVariableName <- paste0(x$observations.observationVariableName, " count")

  z <- rbind(v, w, x)

  y <- reshape2::dcast(z, germplasmName ~ observations.observationVariableName)
  y <- y[, -grep("Tassel", colnames(y))] %>% rename(Clone = germplasmName) }}))

output$colSelect <- renderUI({
  pickerInput(
    inputId = "phenoPick",
    label = "Choose phenotypes to view",
    choices = colnames(performance_init()),
    options = list(`actions-box` = TRUE), multiple = T
  )
})

datasetInput <- eventReactive(input$selectCol, {
  datasetInput <- performance_init() %>%
    select(input$phenoPick)

  return(datasetInput)
})

output$performanceTable <- ({
  renderDT(datasetInput(), extensions = "FixedColumns", options = list(
    scrollX = TRUE, fixedColumns = list(leftColumns = 2)
  ))
})

## Crosses ----

  
crosses_init <- eventReactive(input$makecrosses, withProgress(message = "Pulling Cross Data", {{
  ### for filtering
  germplasm <- as.data.frame(inventory_init())

  ### get
  historical_cross_table <- init_cross_table(cross_list = historical_crosses)

  historical_cross_table2 <- historical_cross_table[which(historical_cross_table$Female.Parent %in% germplasm$Clone &
    historical_cross_table$Male.Parent %in% germplasm$Clone), ]


  new_crosses_table <- init_cross_table(
    cross_list = ba_crosses_study(con = brap2, crossingProjectDbId = crossingprojectdbid, rclass = "data.frame"),
    Female.Parent = "data.parent1.germplasmName", Male.Parent = "data.parent2.germplasmName", new_crosses = T
  )

  new_crosses_table2 <- new_crosses_table[which(new_crosses_table$Female.Parent %in% germplasm$Clone &
    new_crosses_table$Male.Parent %in% germplasm$Clone), ]

  all_cross_table <- rbind(historical_cross_table2, new_crosses_table2)

  rownames(all_cross_table) <- NULL

  all_cross_table[order(all_cross_table[,2], all_cross_table[,1]),]
}}))

output$crossesTable <- ({
  renderDT(crosses_init(), extensions = "FixedColumns", options = list(
    scrollX = TRUE, fixedColumns = list(leftColumns = 3)
  ))
})





## Download page -----



output$downloaddata <- downloadHandler(
  filename = function() {
    paste0(reactive_date(), "_STracT_data.xlsx")
  },
  content = function(file) {
    write_xlsx(list(
      CloneData = list(inventory_init(), merge(pedigree_init(), pedmatrix_init()[, c("LCP85-0384", "Clone")],
        by = "Clone"
      ), performance_init()) %>% reduce(full_join, by = "Clone"),
      RelationshipMatrix=pedmatrix_init(),
      PossibleCrosses = crosses_init()
    ), path = file)
  }
)
  }

shinyApp(ui, server)



