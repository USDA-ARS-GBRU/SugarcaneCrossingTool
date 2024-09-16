#Performance.R
performance_server <- function(input, output, session, reactive_iid, rv, rv_trait_scatter, inventory_init) {
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
}