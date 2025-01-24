#Optimization.R

optimization_server <- function(input, output, session, crossing_plan, inventory_init) {
  
  # Initialize reactive values for optimization results
  rv <- reactiveValues(
    optimization_result = NULL,
    previous_crosses = NULL
  )

  # Cross Optimization
  observeEvent(input$run_optimization, {
    # Get current inventory data
    inventory_data <- as.data.frame(rbind(inventory_init()$male, inventory_init()$female))
    
    # Get selected parents
    male_parent_list <- c(unique(input$male_list))
    female_parent_list <- c(unique(input$female_list))
    
    ck <- input$culling_k
    
    # Check if parents are selected
    if (length(male_parent_list) == 0 || length(female_parent_list) == 0) {
      showNotification("Please select both male and female parents before running optimization.", type = "error")
      return()
    }
    
    # Run optimization
    result <- tryCatch({
      optimize_crosses(inventory_data, 
                      male_parents = male_parent_list,
                      female_parents = female_parent_list,
                      n_crosses = input$n_crosses,
                      max_crosses_per_parent = input$max_crosses_per_parent,
                      culling_k = input$culling_k,
                      blup = blup_data[,1:4],
                      amat = as.matrix(parent_amat),
                      weights = c(input$brix, input$biomass, input$ratoon))
    }, error = function(e) {
      showNotification(paste("Error in optimization:", e$message), type = "error")
      return(list(crosses = data.frame(), plot = NULL))
    })
    
    # Store the original optimization result
    rv$optimization_result <- result
    
    # Create a copy for cubicle management with additional columns
    if (!is.null(result$crosses) && nrow(result$crosses) > 0) {
      cubicle_crosses <- result$crosses
      cubicle_crosses$status <- "Unassigned"  # Add status column
      cubicle_crosses$cubicle_id <- NA        # Add cubicle_id column
      crossing_plan(cubicle_crosses)          # Initialize crossing plan with modified crosses
      
      # Show success notification
      showNotification("Optimization complete. You can now assign crosses to cubicles.", 
                      type = "message")
    }
  })

  # Render optimized crosses table
  output$optimized_crosses_table <- renderDT({
    req(rv$optimization_result)
    crosses <- rv$optimization_result$crosses
    if (!is.null(crosses) && nrow(crosses) > 0) {
      # Join with previous crosses if available
      if (!is.null(rv$previous_crosses)) {
        crosses <- crosses %>%
          left_join(rv$previous_crosses, 
                    by = c("Female.Parent", "Male.Parent"))
      }
      
      # Add rank column
      crosses <- crosses %>%
        mutate(Rank = 1:input$n_crosses)
      
      datatable(crosses, 
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

  # Update the optimization plot to use the original result
  output$optimization_plot <- renderPlotly({
    req(rv$optimization_result)
    plot <- rv$optimization_result$plot
    
    if (!is.null(plot)) {
      # Extract plot data and ensure it has all required columns
      plot_data <- plot$data
      
      # Add Selected column if it doesn't exist
      if (!"Selected" %in% names(plot_data)) {
        # Determine which points are selected based on the actual crosses
        selected_crosses <- rv$optimization_result$crosses
        plot_data$Selected <- FALSE
        
        # Mark points as selected if they match the optimized crosses
        for (i in 1:nrow(selected_crosses)) {
          plot_data$Selected <- plot_data$Selected | 
            (plot_data$Parent1 == selected_crosses$Female.Parent[i] & 
             plot_data$Parent2 == selected_crosses$Male.Parent[i])
        }
      }
      
      # Add hover text
      hover_text <- paste(
        "\nFemale.Parent:", plot_data$Parent1,
        "\nMale.Parent:", plot_data$Parent2,
        "\nSelection Index:", round(plot_data$Y, 3),
        "\nKinship:", round(plot_data$K, 3)
      )
      
      # Create new ggplot with hover text and vertical line
      p <- ggplot(plot_data, aes(x = K, y = Y)) +
        geom_point(aes(color = Selected), size = 3, alpha = 0.7) +
        scale_color_manual(values = c("FALSE" = "gray70", "TRUE" = "#1f77b4")) +
        geom_vline(xintercept = input$culling_k, linetype = "dashed") +
        theme_minimal() +
        labs(
          x = "Kinship Coefficient",
          y = "Selection Index",
          title = paste("Cross Optimization Plot (Selected Crosses Highlighted)"),
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

  # Add download handler
  output$download_optimized_plan <- downloadHandler(
    filename = function() {
      paste("optimized_crossing_plan_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Check if optimized crosses exist
      crosses <- rv$optimization_result$crosses
      if (!is.null(crosses) && nrow(crosses) > 0) {
        writexl::write_xlsx(crosses, path = file)
      } else {
        # If no optimized crosses, create a dummy dataframe with a message
        dummy_data <- data.frame(Message = "No optimized crosses available. Please run the optimization first.")
        writexl::write_xlsx(dummy_data, path = file)
      }
    }
  )

  # Return reactive values for use in other modules
  return(list(
    optimization_result = reactive({ rv$optimization_result }),
    crossing_plan = crossing_plan
  ))
} 