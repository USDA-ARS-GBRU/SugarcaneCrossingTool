#Cubicle.R

cubicle_server <- function(input, output, session, crossing_plan) {
  
  # Initialize reactive values for cubicle management
  rv <- reactiveValues(
    cubicles = list(),
    next_cubicle_id = 1
  )

  # Render the crossing table
  output$crossing_table <- renderDT({
    crosses_data <- crossing_plan()
    
    if (is.null(crosses_data) || nrow(crosses_data) == 0) {
      return(datatable(
        data.frame(Message = "No crosses available. Please run optimization first."),
        options = list(pageLength = 10)
      ))
    }
    
    datatable(
      crosses_data,
      selection = 'multiple',
      options = list(
        pageLength = 10,
        searching = TRUE,
        ordering = TRUE
      )
    )
  })

  # Create new cubicle
  observeEvent(input$create_cubicle, {
    req(crossing_plan())
    selected_rows <- input$crossing_table_rows_selected
    
    if (length(selected_rows) == 0) {
      showNotification(
        "Please select crosses first",
        type = "warning",
        duration = 5
      )
      return()
    }
    
    selected_crosses <- crossing_plan()[selected_rows, ]
    
    # Check if all selected crosses have the same male parent
    if (length(unique(selected_crosses$Male.Parent)) > 1) {
      showNotification(
        "All selected crosses must have the same male parent",
        type = "warning",
        duration = 5
      )
      return()
    }

    # Create new cubicle
    new_cubicle <- list(
      id = rv$next_cubicle_id,
      male = unique(selected_crosses$Male.Parent),
      crosses = selected_crosses,
      pollination_date = input$pollination_date,
      processing_date = input$processing_date,
      notes = ""
    )
    
    rv$cubicles[[length(rv$cubicles) + 1]] <- new_cubicle
    rv$next_cubicle_id <- rv$next_cubicle_id + 1
    
    showNotification(
      "Cubicle created successfully!",
      type = "message",
      duration = 5
    )
  })

  # Display cubicle table
  output$cubicle_table <- renderDT({
    current_cubicles <- rv$cubicles
    
    if (length(current_cubicles) == 0) {
      return(NULL)
    }
    
    cubicle_df <- do.call(rbind, lapply(current_cubicles, function(cubicle) {
      data.frame(
        Cubicle_ID = cubicle$id,
        Male = cubicle$male,
        Females = paste(cubicle$crosses$Female.Parent, collapse = ", "),
        Pollination_Date = format(as.Date(cubicle$pollination_date), "%Y-%m-%d"),
        Processing_Date = format(as.Date(cubicle$processing_date), "%Y-%m-%d"),
        Notes = cubicle$notes,
        stringsAsFactors = FALSE
      )
    }))
    
    datatable(
      cubicle_df,
      editable = list(target = "cell", disable = list(columns = c(1:5))),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      )
    )
  })

  # Handle saving/loading layouts
  output$save_data <- downloadHandler(
    filename = function() {
      paste("cubicle_management_", format(Sys.Date(), "%Y%m%d"), ".xlsx", sep = "")
    },
    content = function(file) {
      current_cubicles <- rv$cubicles
      
      if (length(current_cubicles) == 0) {
        cubicle_df <- data.frame(
          Cubicle_ID = character(),
          Male = character(),
          Females = character(),
          Pollination_Date = character(),
          Processing_Date = character(),
          Notes = character(),
          stringsAsFactors = FALSE
        )
      } else {
        cubicle_df <- do.call(rbind, lapply(current_cubicles, function(cubicle) {
          data.frame(
            Cubicle_ID = cubicle$id,
            Male = cubicle$male,
            Females = paste(cubicle$crosses$Female.Parent, collapse = ", "),
            Pollination_Date = format(as.Date(cubicle$pollination_date), "%Y-%m-%d"),
            Processing_Date = format(as.Date(cubicle$processing_date), "%Y-%m-%d"),
            Notes = cubicle$notes,
            stringsAsFactors = FALSE
          )
        }))
      }
      writexl::write_xlsx(cubicle_df, path = file)
    }
  )
} 