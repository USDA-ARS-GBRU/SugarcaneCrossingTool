library(shiny)
library(shinyjs)
library(DT)  # For interactive tables
library(plotly)  # For visualization
library(readxl)  # Add this for Excel support

ui <- fluidPage(
    useShinyjs(),
    # Add CSS
    tags$head(
        tags$style(HTML("
            .dataTables_wrapper {
                margin-top: 20px;
                margin-bottom: 20px;
            }
            .well {
                margin: 10px 0;
                padding: 15px;
            }
        "))
    ),
    titlePanel("Breeding Cubicle Manager"),
    
    sidebarLayout(
        sidebarPanel(
            # Upload optimized crossing plan
            fileInput("crossing_plan", "Upload Optimized Crossing Plan (CSV/Excel)",
                     accept = c(
                         "text/csv",
                         "application/vnd.ms-excel",
                         "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                         ".csv", ".xls", ".xlsx"
                     )),
            
            # Create cubicle button
            actionButton("create_cubicle", "Create Cubicle from Selected Crosses"),
            
            hr(),
            
            # Instructions
            helpText("1. Upload your optimized crossing plan"),
            helpText("2. Select up to 3 crosses with the same male"),
            helpText("3. Click 'Create Cubicle' to group them"),
            helpText("4. Track your progress in the summary"),
            
            # Dates
            dateInput("planting_date", "Planting Date:", value = Sys.Date()),
            dateInput("expected_flowering", "Expected Flowering:", value = Sys.Date() + 60)
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Crossing Plan",
                    # Visualization of crossing plan
                    plotlyOutput("crossing_plot"),
                    
                    # Crossing plan table with status
                    h4("Available Crosses"),
                    DTOutput("crossing_table")
                ),
                
                tabPanel("Cubicle Layout",
                    # Container for dynamically created cubicles
                    uiOutput("cubicles_container"),
                    
                    # Summary statistics
                    fluidRow(
                        column(12,
                            h3("Summary Statistics"),
                            verbatimTextOutput("statistics")
                        )
                    )
                )
            )
        )
    )
)

server <- function(input, output, session) {
    # Store crossing plan data with status
    crossing_plan <- reactiveVal(NULL)
    
    # Read crossing plan and add status column
    observeEvent(input$crossing_plan, {
        req(input$crossing_plan)
        
        # Get file extension
        ext <- tools::file_ext(input$crossing_plan$name)
        
        # Read file based on extension
        plan_data <- tryCatch({
            if (ext == "csv") {
                read.csv(input$crossing_plan$datapath)
            } else if (ext %in% c("xls", "xlsx")) {
                readxl::read_excel(input$crossing_plan$datapath)
            } else {
                stop("Unsupported file format")
            }
        }, error = function(e) {
            showNotification(
                paste("Error reading file:", e$message),
                type = "error",
                duration = NULL
            )
            return(NULL)
        })
        
        # Add debug print after loading
        if (!is.null(plan_data)) {
            print("Data loaded successfully:")
            print(str(plan_data))
            print(head(plan_data))
            
            # Standardize column names
            names(plan_data) <- gsub("Female.Parent", "female", names(plan_data))
            names(plan_data) <- gsub("Male.Parent", "male", names(plan_data))
            names(plan_data) <- gsub("Y", "selection_index", names(plan_data))
            names(plan_data) <- gsub("K", "kinship_coefficient", names(plan_data))
            
            print("Column names after standardization:")
            print(names(plan_data))
            
            plan_data$status <- "Unassigned"  # Add status column
            plan_data$cubicle_id <- NA        # Track cubicle assignment
            crossing_plan(as.data.frame(plan_data))  # Ensure data.frame format
        }
    })
    
    # Display interactive crossing plan plot
    output$crossing_plot <- renderPlotly({
        req(crossing_plan())
        plan_data <- crossing_plan()
        
        # Check which columns are available
        available_cols <- names(plan_data)
        
        # Default plot if kinship and selection index are available
        if ("kinship_coefficient" %in% available_cols && "selection_index" %in% available_cols) {
            p <- plot_ly(plan_data, 
                         x = ~kinship_coefficient,
                         y = ~selection_index,
                         color = ~status,
                         text = ~paste("Female:", female,
                                     "<br>Male:", male,
                                     "<br>Status:", status),
                         type = "scatter",
                         mode = "markers") %>%
                layout(title = "Crossing Plan Status",
                       xaxis = list(title = "Kinship Coefficient"),
                       yaxis = list(title = "Selection Index"))
        } else {
            # Fallback plot showing just male/female combinations
            p <- plot_ly(plan_data,
                         x = ~as.factor(male),
                         y = ~as.factor(female),
                         color = ~status,
                         text = ~paste("Female:", female,
                                     "<br>Male:", male,
                                     "<br>Status:", status),
                         type = "scatter",
                         mode = "markers") %>%
                layout(title = "Crossing Plan Status",
                       xaxis = list(title = "Male Parent"),
                       yaxis = list(title = "Female Parent"))
        }
        p
    })
    
    # Display crossing plan table with status
    output$crossing_table <- renderDT({
        req(crossing_plan())
        plan_data <- crossing_plan()
        
        print("Attempting to render table with data:")
        print(str(plan_data))
        
        # Simplify the table rendering first to ensure it works
        datatable(
            plan_data,
            selection = 'multiple',
            options = list(
                pageLength = 10,
                searching = TRUE,
                ordering = TRUE
            )
        )
    })
    
    # Modified cubicle creation to use selected crosses
    observeEvent(input$create_cubicle, {
        req(crossing_plan())
        selected_rows <- input$crossing_table_rows_selected
        
        if (length(selected_rows) == 0) {
            showNotification("Please select crosses first", type = "error")
            return()
        }
        
        if (length(selected_rows) > 3) {
            showNotification("Maximum 3 crosses per cubicle", type = "error")
            return()
        }
        
        selected_crosses <- crossing_plan()[selected_rows, ]
        unique_males <- unique(selected_crosses$male)
        
        if (length(unique_males) > 1) {
            showNotification("All crosses in a cubicle must share the same male", type = "error")
            return()
        }
        
        # Create new cubicle
        current_cubicles <- cubicles()
        new_cubicle_id <- length(current_cubicles) + 1
        
        new_cubicle <- list(
            id = new_cubicle_id,
            male = unique_males[1],
            crosses = selected_crosses,
            notes = ""
        )
        
        # Update crossing plan status
        plan_data <- crossing_plan()
        plan_data$status[selected_rows] <- "Assigned"
        plan_data$cubicle_id[selected_rows] <- new_cubicle_id
        crossing_plan(plan_data)
        
        # Add new cubicle
        current_cubicles[[new_cubicle_id]] <- new_cubicle
        cubicles(current_cubicles)
    })
    
    # Store cubicle data
    cubicles <- reactiveVal(list())
    
    # Render cubicles
    output$cubicles_container <- renderUI({
        current_cubicles <- cubicles()
        
        if (length(current_cubicles) == 0) {
            return(h4("No cubicles created yet"))
        }
        
        cubicle_elements <- lapply(current_cubicles, function(cubicle) {
            div(
                class = "well",
                style = "margin: 10px 0;",
                
                # Cubicle header
                h4(paste("Cubicle", cubicle$id)),
                
                # Male cultivar display
                p(strong("Male: "), cubicle$male),
                
                # Display crosses
                div(
                    class = "crosses-container",
                    lapply(1:nrow(cubicle$crosses), function(i) {
                        cross <- cubicle$crosses[i,]
                        p(sprintf("Cross %d: %s × %s", i, cross$female, cross$male))
                    })
                ),
                
                textAreaInput(paste0("notes_", cubicle$id), 
                             "Notes:", 
                             value = cubicle$notes),
                
                actionButton(paste0("delete_", cubicle$id), "Delete Cubicle", 
                             class = "btn-danger"),
                
                div(
                    class = "date-info",
                    p(strong("Planting Date: "), format(input$planting_date, "%B %d, %Y")),
                    p(strong("Expected Flowering: "), format(input$expected_flowering, "%B %d, %Y"))
                )
            )
        })
        
        do.call(tagList, cubicle_elements)
    })
    
    # Handle ratio displays for each cubicle
    observe({
        current_cubicles <- cubicles()
        
        for (cubicle in current_cubicles) {
            local({
                cubicle_id <- cubicle$id
                
                output[[paste0("ratio_", cubicle_id)]] <- renderUI({
                    female_count <- input[[paste0("female_count_", cubicle_id)]]
                    ratio <- female_count / 1  # 1 male
                    
                    ratio_color <- if (ratio > 3) "red" else "black"
                    
                    p(
                        style = paste0("color: ", ratio_color, ";"),
                        sprintf("Female to Male Ratio: %.1f:1", ratio)
                    )
                })
            })
        }
    })
    
    # Handle print button
    observeEvent(input$print_layout, {
        # Create a formatted layout for printing
        layout_html <- div(
            h2("Breeding Cubicle Layout"),
            p("Generated on: ", format(Sys.time(), "%B %d, %Y")),
            hr(),
            lapply(cubicles(), function(cubicle) {
                div(
                    style = "border: 1px solid black; padding: 10px; margin: 10px 0;",
                    h4(paste("Cubicle", cubicle$id)),
                    p(strong("Male: "), cubicle$male),
                    p(strong("Females: "), cubicle$female_count),
                    p(strong("Notes: "), input[[paste0("notes_", cubicle$id)]])
                )
            })
        )
        
        # Open in new window for printing
        showModal(modalDialog(
            layout_html,
            footer = tagList(
                actionButton("print_now", "Print"),
                modalButton("Close")
            )
        ))
    })
    
    observeEvent(input$print_now, {
        runjs("window.print();")
    })
    
    observe({
        current_cubicles <- cubicles()
        lapply(current_cubicles, function(cubicle) {
            observeEvent(input[[paste0("delete_", cubicle$id)]], {
                current_cubicles <- cubicles()
                current_cubicles[[cubicle$id]] <- NULL
                cubicles(current_cubicles)
            })
        })
    })
    
    output$save_data <- downloadHandler(
        filename = function() {
            paste0("cubicle_layout_", Sys.Date(), ".rds")
        },
        content = function(file) {
            saveRDS(cubicles(), file)
        }
    )
    
    observeEvent(input$load_data, {
        req(input$load_data)
        loaded_cubicles <- readRDS(input$load_data$datapath)
        cubicles(loaded_cubicles)
    })
    
    # Enhanced statistics output
    output$statistics <- renderText({
        plan_data <- crossing_plan()
        if (is.null(plan_data)) return("No crossing plan loaded")
        
        total_crosses <- nrow(plan_data)
        assigned_crosses <- sum(plan_data$status == "Assigned")
        remaining_crosses <- total_crosses - assigned_crosses
        
        paste0(
            "Total Crosses in Plan: ", total_crosses, "\n",
            "Assigned to Cubicles: ", assigned_crosses, "\n",
            "Remaining to Assign: ", remaining_crosses, "\n",
            "Number of Cubicles: ", length(cubicles()), "\n",
            "Progress: ", round(assigned_crosses/total_crosses * 100, 1), "%"
        )
    })
    
    observe({
        current_cubicles <- cubicles()
        if (length(current_cubicles) > 0) {
            total_females <- sum(sapply(current_cubicles, function(x) {
                count <- input[[paste0("female_count_", x$id)]]
                if (is.null(count)) return(0)
                as.numeric(count)
            }))
            
            if (total_females > 100) {
                showNotification(
                    "Warning: Total female count exceeds 100", 
                    type = "warning",
                    duration = NULL
                )
            }
        }
    })
}

shinyApp(ui = ui, server = server)
