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
library(networkD3)

tags$head(
  tags$script(src = "https://d3js.org/d3.v5.min.js"),
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/networkD3/0.4.1/networkD3.min.js")
)


## CUSTOM DATA and FUNCTION LOAD -----------

source("app_functions.R")
source("app_configs.R")

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
    
    textInput("inventoryid", "Login with your IID:", value=""),
    textInput("crossesid", "Login with your CID:", value=""),
    
    dateInput(
      "date",
      "Choose A Date:"
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
        p("Welcome to STracT, the Sugarcane crossing tool! This tool lets you 
           see data and generate reports for all of the 
          clones that are flowering on a specific day."),
        h4("Here are instructions on how to use this tool."),
        p("1. Choose a date from the calendar on the 
          left for the day you want to see"),
        p(
          "2. Click 'Get Flower Inventory Data' to pull in data from",
          a(href = "https://sugarcanebase.breedinginsight.net/", "SugarcaneBase"),
          ". This will show you an inventory list of flowering clones in the 'Flowering' tab to the left"
        ),
        p("3. Next, you can click on any of the other tabs to see their associated data"),
        p("4. You can also download a report using the 'Download' tab."),
        h4("Note, you've logged in to view inventory for these can lines: "),
        textOutput("inventoryPointer"),
        
        h4("Note, you've logged in to track this crossing experiment: "),
        textOutput("crossPointer"),
        
        # h4("Note, you're getting inventory for this date:"),
        # textOutput("datePointer")
        
      ),

      ### Flowering tab content -----
      tabItem(
        tabName = "flowering",
        fluidRow(
          box(p("This table shows you the count and sex of each clone that is flowering on the day you selected.")),
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
      uiOutput("pedigreeGraphUI"),
      networkD3::forceNetworkOutput(outputId = "pedigreeGraph")
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
              downloadButton("downloaddata", "Download Data")),
          
        )
      )
    )
  )
)








# SERVER ---------------------------------------

server <- function(input, output, session) {
  library(networkD3)
  # choose date
  reactive_date <- reactive({
    input$date
  })


pedigree_data <- reactiveVal()

observeEvent(pedigree_init(), {
  pedigree_data(pedigree_init())
})

  rv <- reactiveValues(selectedColumns = NULL)
  rv_trait_scatter <- reactiveValues(selectedColumns = NULL)

  selectedClone <- reactiveVal()

  

  
  
  # Update selectedColumns when the user selects new columns in the Performance tab
  observeEvent(input$selectCol, {
    rv$selectedColumns <- input$phenoPick
    rv_trait_scatter$selectedColumns <- input$phenoPick
  })
  
  observeEvent(input$selectedClone, {
  selectedClone <- input$selectedClone
  updateSelectInput(session, "pedigreeGraphUI")
})


  observe({
  phenotypes <- input$phenoPick

  # Update X-axis dropdown
  updateSelectInput(session, "xAxis_scatter", choices = phenotypes, selected = phenotypes[1])

  # Update Y-axis dropdown
  updateSelectInput(session, "yAxis_scatter", choices = phenotypes, selected = phenotypes[2])
})

observeEvent(input$selectCol_scatter, {
  rv_trait_scatter$xAxis <- input$xAxis_scatter
  rv_trait_scatter$yAxis <- input$yAxis_scatter
})

  observeEvent(input$selectCol_scatter, {
    rv_trait_scatter$selectedColumns <- input$phenoPick_scatter
  })


  reactive_iid<-reactive({input$inventoryid})
  
  reactive_cid<-reactive({input$crossesid})
  
  #output$datePointer<-renderText(input$date)

  # point to current study
  
  output$inventoryPointer <- renderText({
   validate(
     need(input$inventoryid != "", "Please log in with your IID")
   )
   unique(brapi::ba_studies_table(con = brap, studyDbId = as.character(input$inventoryid))$studyName)
 })

 output$crossPointer <- renderText({
   validate(
     need(input$crossesid != "", "Please log in with your CID")
   )
   unique(ba_crosses_study(con = brap2, crossingProjectDbId = as.character(input$crossesid), rclass = "data.frame")$data.crossingProjectName[[1]])
 })

  
  
  ## Flowering  ------

  inventory_init <- eventReactive(input$brapipull, withProgress(message = "Pulling Inventory Data", {{ inven <- data.frame(brapi::ba_studies_table(con = brap, studyDbId = reactive_iid(), rclass="data.frame")) %>%
    filter(observationLevel == "plant") %>% # select just plant rows
    set_names(~(.)%>% str_replace_all("SUGARCANE.*","") %>% str_replace_all("\\.","")) %>%  # take CO term out of colnames
    filter(FloweringTime== reactive_date()) %>% 
    select(germplasmName, germplasmDbId, SexMFWM) %>% 
    group_by(germplasmName, germplasmDbId, SexMFWM) %>% 
    summarise(count = n()) %>%
    rename(Clone = germplasmName, FloweringCount = count, Sex = SexMFWM) }}))

  output$inventoryTable <- ({
    renderDT(inventory_init()[,-which(colnames(inventory_init())=="germplasmDbId")], options = list(language = list(
      zeroRecords = "There are no records to display. Double check the date you selected and try again. 
    You may need to wait a few minutes if inventory records were recently uploaded"
    )))
  })
  
  # take the away for now
  # , editable=list(target="column", disable=list(columns=c(0:2)))

  ## Pedigree and Progeny ----

  pedigree_init <- eventReactive(input$makepedigree, withProgress(message = "Pulling Progeny Data", {{ germplasm <- as.data.frame(inventory_init())
    #germplasm<-inven #for testing
    germplasm<-germplasm[duplicated(germplasm$Clone)==FALSE,]
    selectedClone <- reactiveVal()
    tmp <- stripClass(
      as.data.frame(
        ba_germplasm_details2(con = brap2, germplasmQuery = as.character(paste0("?studyDbId=", reactive_iid(), "&pageSize=1000")), rclass = "data.frame")
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
  
    germplasm<-germplasm[duplicated(germplasm$Clone)==FALSE,]
  
    mat <- PedMatrix(pedigree_download)

    if ("LCP85-0384" %in% germplasm$Clone) {
      axis <- germplasm$Clone
    } else {
      axis <- c(germplasm$Clone, "LCP85-0384")
    }

    mat2 <- round(mat[axis, axis], 2) # subset

    mat2 <- as.data.frame(mat2) # format for downstream
    mat2$Clone <- rownames(mat2) # add rownames
    mat2<-mat2[,c(dim(mat2)[2],1:dim(mat2)[2]-1)] #reorder
    return(mat2) }})

  output$pedigreeTable <- ({
    renderDT(merge(pedigree_init(), pedmatrix_init()[, c("LCP85-0384", "Clone")],
      by = "Clone"
    ) %>% rename(Rel.2.LCP850384 = "LCP85-0384"), options = list(language = list(
      zeroRecords = "There are no pedigree records to display. Double check that there are inventory records for the date you selected"
    )))
  }) # this merge fufills user request to see similarity to LCP85-

  output$cloneDropdown <- renderUI({
  pedigree_data_val <- pedigree_data()
  selectInput("selectedClone", "Select a Clone", choices = unique(pedigree_data_val$Clone))
})

output$pedigreeGraph <- renderUI({
  req(selectedClone())  # Ensure selectedClone has a value

  pedigree_data_val <- pedigree_data()  # Get the current value of pedigree_data
  selected_clone_val <- input$selectedClone  # Get the current value of selectedClone

  print(pedigree_data_val)  # Debug: Print pedigree_data_val
  print(selected_clone_val)  # Debug: Print selected_clone_val

  if (!is.null(pedigree_data_val) && nrow(pedigree_data_val) > 0) {
    subset_data <- pedigree_data_val[pedigree_data_val$Clone == selected_clone_val, ]

    if (nrow(subset_data) > 0) {
      # Create a data frame for the network graph
      network_data <- data.frame(
        from = subset_data$Clone,
        to = subset_data$Pedigree,
        value = 1,
        group = ifelse(grepl("F", subset_data$Pedigree), "Female", "Male")
      )

      # Debug: Print network_data
      print(network_data)

      # Create a force network graph
      graph <- forceNetwork(
        Links = network_data,
        Nodes = data.frame(name = unique(c(network_data$from, network_data$to))),
        Source = "from",
        Target = "to",
        Value = "value",
        NodeID = "name",
        Group = "group",
        linkWidth = 2,
        opacity = 0.9,
        zoom = TRUE,
        legend = TRUE,
        legendSource = "inline",
        nodeColour = JS(
          'function(node) {
             return node.group === "Female" ? "#FF0000" : "#0000FF";
           }'
        ),
        nodeWidth = 20,
        fontSize = 12
      )

      # Debug: Print graph
      print(graph)

      return(graph)
    } else {
      print("No data for the selected clone")
    }
  } else {
    print("No pedigree data available")
  }
})

# Add this to update the UI with the rendered forceNetwork graph
output$pedigreeGraphUI <- renderUI({
  pedigreeGraph <- input$pedigreeGraph
  if (!is.null(pedigreeGraph)) {
    pedigreeGraph
  }
})







output$pedigreeMatrix <- renderPlotly({
  pedmatrix_data <- pedmatrix_init()
  heatmaply(pedmatrix_data)
})



  ## Performance ----

performance_init <- eventReactive(input$makeperformance, withProgress(message = "Pulling Performance Data", {
  germplasm <- as.data.frame(inventory_init())
  germplasm <- germplasm[duplicated(germplasm$Clone) == FALSE,]

  # pull phenotype data for each item in inventory table ('germplasm')
  tmp <- lapply(germplasm$germplasmDbId, function(dbId) {
    tryCatch(
      {
        ba_phenotypes_search(
          con = brap,
          germplasmDbId = as.character(dbId),
          rclass = "data.frame",
          observationLevel = "plot",
          pageSize = 20000
        ) %>%
          as.data.frame()  # Convert tibbles to data frames
      },
      error = function(e) {
        cat("Error in ba_phenotypes_search for germplasmDbId =", dbId, "\n")
        return(NULL)
      }
    )
  })


  # if item does not have any phenotype data, remove it from the list
  tmp <- Filter(function(x) !is.null(x) && length(dim(x)) > 0, tmp)

  # bind all list elements together into one table
  tmp <- bind_rows(tmp)

  # extract stage information from study name (Needs to be updated)
  tmp$Advanced <- str_extract(tmp$studyName, "S3|S4|Stage 2|OUTFIELD|INFIELD|NURSERY")

  # aggregate 'Advanced' data by germplasm name
  s <- aggregate(Advanced ~ germplasmName, unique, data = tmp)

  # clean up -- remove quotes and parentheses from output
  s$Advanced <- gsub("c\\(|\\)", "", s$Advanced)
  s$Advanced <- noquote(gsub('"', "", s$Advanced))

  # aggregate (function = mean) all other phenotypic data by germplasm name,
  v <- aggregate(
    as.numeric(observations.value) ~ observations.observationVariableName + germplasmName,
    mean,
    data = tmp,
    na.action = na.omit
  )
  colnames(v)[3] <- "observations.value"
  v$observations.value <- round(as.numeric(v$observations.value), 2)
  v$observations.observationVariableName <- paste0(v$observations.observationVariableName, " mean")

  # aggregate (function = standard deviation) all other phenotypic data by germplasm name,
  w <- aggregate(
    observations.value ~ observations.observationVariableName + germplasmName,
    sd,
    data = tmp,
    na.action = na.omit
  )
  w$observations.value <- round(as.numeric(w$observations.value), 2)
  w$observations.observationVariableName <- paste0(w$observations.observationVariableName, " sd")

  # aggregate (function = count of observations) all other phenotypic data by germplasm name,
  x <- aggregate(
    observations.value ~ observations.observationVariableName + germplasmName,
    length,
    data = tmp,
    na.action = na.omit
  )
  x$observations.value <- round(as.numeric(x$observations.value), 2)
  x$observations.observationVariableName <- paste0(x$observations.observationVariableName, " count")

  # bind all numeric data together
  z <- rbind(v, w, x)


  # reshape
  y <- reshape2::dcast(z, germplasmName ~ observations.observationVariableName)

  # join with Advanced data
  s <- s %>% right_join(y) %>% rename(Clone = germplasmName)

  return(s)
}))

# the rest of this is the column picker
output$colSelect <- renderUI({
  pickerInput(
    inputId = "phenoPick",
    label = "Choose phenotypes to view",
    choices = colnames(performance_init()),
    options = list(`actions-box` = TRUE),
    multiple = TRUE
  )
})

datasetInput <- eventReactive(input$selectCol, {
  selectedColumns <- c("Clone", input$phenoPick)
  datasetInput <- performance_init() %>%
    select(selectedColumns)

  return(datasetInput)
})

output$performanceTable <- renderDT({
  datasetInput() %>%
    select(Clone, everything())
}, extensions = "FixedColumns", options = list(
  scrollX = TRUE, fixedColumns = list(leftColumns = 2)
))


# create scatter plot
datasetInput_scatter <- eventReactive(input$selectCol_scatter, {
  datasetInput_scatter <- performance_init() %>%
    select(input$xAxis_scatter, input$yAxis_scatter)

  return(datasetInput_scatter)
})

output$performanceTable_scatter <- renderDT({
  datasetInput_scatter() %>%
    select(Clone, everything())
}, extensions = "FixedColumns", options = list(
  scrollX = TRUE, fixedColumns = list(leftColumns = 2)
))



output$traitScatterPlot <- renderPlotly({
  req(input$xAxis_scatter, input$yAxis_scatter)

  rv_trait_scatter$xAxis <- input$xAxis_scatter
  rv_trait_scatter$yAxis <- input$yAxis_scatter

  if (!is.null(rv_trait_scatter$xAxis) && !is.null(rv_trait_scatter$yAxis)) {
    selectedData <- performance_init() %>%
      select(Clone, rv_trait_scatter$xAxis, rv_trait_scatter$yAxis)

    x_col <- as.name(rv_trait_scatter$xAxis)
    y_col <- as.name(rv_trait_scatter$yAxis)

    plot_ly(
      data = selectedData,
      x = ~selectedData[[x_col]],
      y = ~selectedData[[y_col]],
      type = 'scatter',
      mode = 'markers',
      text = ~Clone,
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Trait Scatter Plot",
        xaxis = list(title = rv_trait_scatter$xAxis),
        yaxis = list(title = rv_trait_scatter$yAxis)
      )
  }
})

output$scatterPlotDropdown_x <- renderUI({
  selectInput(
    inputId = "xAxis_scatter",
    label = "Choose X-axis",
    choices = colnames(performance_init()),
    selected = rv_trait_scatter$xAxis
  )
})

output$scatterPlotDropdown_y <- renderUI({
  selectInput(
    inputId = "yAxis_scatter",
    label = "Choose Y-axis",
    choices = colnames(performance_init()),
    selected = rv_trait_scatter$yAxis
  )
})
  
    


  ## Crosses ----


  crosses_init <- eventReactive(input$makecrosses, withProgress(message = "Pulling Cross Data", {{
    ### for filtering
    germplasm <- as.data.frame(inventory_init())
    germplasm<-germplasm[duplicated(germplasm$Clone)==FALSE,]
    
    ### get
    historical_cross_table <- InitCrossTable(cross_list = historical_crosses)

    historical_cross_table2 <- historical_cross_table[which(historical_cross_table$Female.Parent %in% germplasm$Clone &
      historical_cross_table$Male.Parent %in% germplasm$Clone), ]


    new_crosses_table <- InitCrossTable(
      cross_list = ba_crosses_study(con = brap2, crossingProjectDbId = reactive_cid(), rclass = "data.frame"),
      Female.Parent = "data.parent1.germplasmName", Male.Parent = "data.parent2.germplasmName", new_crosses = T
    )

    new_crosses_table2 <- new_crosses_table[which(new_crosses_table$Female.Parent %in% germplasm$Clone &
      new_crosses_table$Male.Parent %in% germplasm$Clone), ]

    all_cross_table <- rbind(historical_cross_table2, new_crosses_table2)

    rownames(all_cross_table) <- NULL

    all_cross_table[order(all_cross_table[, 2], all_cross_table[, 1]), ]
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
        RelationshipMatrix = pedmatrix_init(),
        PreviousCrosses = crosses_init()
      ), path = file)
    }
  )
}

shinyApp(ui, server)