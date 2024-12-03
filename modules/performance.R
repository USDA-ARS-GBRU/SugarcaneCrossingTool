#Performance.R
performance_server <- function(input, output, session, reactive_iid, rv, rv_trait_scatter, inventory_init, clone_assignments) {

  performance_init <- eventReactive(input$makeperformance, {
    withProgress(message = "Pulling Performance Data", {
      tryCatch({
        # Validate inputs
        req(inventory_init())
        
         # Filter the inventory data to get unique clones
    germplasm <- as.data.frame(rbind(inventory_init()$male, inventory_init()$female))
    germplasm<-germplasm[duplicated(germplasm$Clone)==FALSE,]
        
        if(nrow(germplasm) == 0) {
          stop("No inventory data available")
        }
        
        # Only show sorted clones
        display <- c(input$male_list, input$female_list)
        if(length(display) == 0) {
          stop("No clones sorted into male/female categories")
        }
        
        germplasm <- germplasm[which(germplasm$Clone %in% display),]
        
        # Pull phenotype data with progress tracking
        total <- length(germplasm$germplasmDbId)
        tmp <- lapply(seq_along(germplasm$germplasmDbId), function(i) {
          incProgress(1/total, detail = paste("Processing clone", i, "of", total))
          dbId <- germplasm$germplasmDbId[i]
          tryCatch({
            pheno_data <- ba_phenotypes_search(
              con = brap,
              germplasmDbId = as.character(dbId),
              rclass = "data.frame",
              observationLevel = "plot",
              pageSize = 20000
            )
            
            if(is.data.frame(pheno_data)) {
              # Clean the observations.value column
              pheno_data$observations.value <- trimws(as.character(pheno_data$observations.value))
              
              # Remove any empty or invalid values
              pheno_data <- pheno_data[!is.na(pheno_data$observations.value) & 
                                      pheno_data$observations.value != "" & 
                                      !is.null(pheno_data$observations.value), ]
              
              # Convert to numeric, with better error handling
              pheno_data$observations.value <- suppressWarnings(
                as.numeric(pheno_data$observations.value)
              )
              
              # Remove any rows where conversion failed
              valid_rows <- !is.na(pheno_data$observations.value)
              if(!all(valid_rows)) {
                warning(sprintf("Removed %d invalid numeric values for clone %s", 
                               sum(!valid_rows), dbId))
              }
              pheno_data <- pheno_data[valid_rows, ]
              
              return(pheno_data)
            } else {
              warning(sprintf("Unexpected data format for clone %s: %s", 
                             dbId, class(pheno_data)))
              return(NULL)
            }
          }, error = function(e) {
            warning(sprintf("Error fetching phenotypes for ID %s: %s", dbId, e$message))
            NULL
          })
        })
        
        # Remove NULL entries and empty data frames
        tmp <- Filter(function(x) !is.null(x) && nrow(x) > 0, tmp)
        
        if(length(tmp) == 0) {
          stop("No phenotype data available for selected clones")
        }

        # Bind all data frames together
        tmp <- do.call(rbind, tmp)
        
        # Extract stage information
        tmp$Advanced <- str_extract(tmp$studyName, "S3|S4|Stage 2|OUTFIELD|INFIELD|NURSERY")
        
        # Ensure observations.value is numeric
        tmp$observations.value <- as.numeric(as.character(tmp$observations.value))
        
        # Aggregate data
        s <- aggregate(Advanced ~ germplasmName, unique, data = tmp)
        s$Advanced <- gsub("c\\(|\\)", "", s$Advanced)
        s$Advanced <- noquote(gsub('"', "", s$Advanced))
        
        # Calculate means
        v <- aggregate(
          observations.value ~ observations.observationVariableName + germplasmName,
          mean,
          data = tmp,
          na.action = na.omit
        )
        v$observations.value <- round(v$observations.value, 2)
        v$observations.observationVariableName <- paste0(v$observations.observationVariableName, " mean")
        
        # Calculate standard deviations
        w <- aggregate(
          observations.value ~ observations.observationVariableName + germplasmName,
          sd,
          data = tmp,
          na.action = na.omit
        )
        w$observations.value <- round(w$observations.value, 2)
        w$observations.observationVariableName <- paste0(w$observations.observationVariableName, " sd")
        
        # Calculate counts
        x <- aggregate(
          observations.value ~ observations.observationVariableName + germplasmName,
          length,
          data = tmp,
          na.action = na.omit
        )
        x$observations.value <- round(x$observations.value, 2)
        x$observations.observationVariableName <- paste0(x$observations.observationVariableName, " count")
        
        # Combine all statistics
        z <- rbind(v, w, x)
        
        # Reshape the data
        y <- reshape2::dcast(z, germplasmName ~ observations.observationVariableName)
        
        # Join with Advanced data and rename columns
        s <- s %>% 
          right_join(y) %>% 
          rename(Clone = germplasmName)
        
        return(s)
        
      }, error = function(e) {
        showNotification(paste("Error getting performance data:", e$message), type = "error", duration = NULL)
        return(data.frame())
      }, warning = function(w) {
        showNotification(paste("Warning during data processing:", w$message), 
                        type = "warning",
                        duration = 10)
      })
    })
  })

  # Add error handling to outputs
  output$colSelect <- renderUI({
    tryCatch({
      req(performance_init())
      pickerInput(
        inputId = "phenoPick",
        label = "Choose phenotypes to view",
        choices = colnames(performance_init()),
        options = list(`actions-box` = TRUE),
        multiple = TRUE
      )
    }, error = function(e) {
      showNotification("Error creating phenotype selector", type = "error")
    })
  })

  datasetInput <- eventReactive(input$selectCol, {
    selectedColumns <- c("Clone", input$phenoPick)
    datasetInput <- performance_init() %>%
      select(selectedColumns)

    return(datasetInput)
  })

  output$performanceTable <- renderDT({
    tryCatch({
      req(datasetInput())
      datasetInput() %>% select(Clone, everything())
    }, error = function(e) {
      data.frame()
    })
  }, extensions = "FixedColumns", options = list(
    scrollX = TRUE, 
    fixedColumns = list(leftColumns = 2)
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
    tryCatch({
      req(input$xAxis_scatter, input$yAxis_scatter, performance_init())
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
