# Install required packages if not already installed
if (!requireNamespace("openxlsx", quietly = TRUE)) {
    install.packages("openxlsx")
}

library(shiny)
library(shinyjs)
library(DT)  # For interactive tables
library(plotly)  # For visualization
library(readxl)  # Add this for Excel support
library(openxlsx)  # Add this with other library imports

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
                    # Table for cubicle layout
                    DTOutput("cubicle_table"),
                    
                    # Notes editor modal will be triggered from table
                    
                    # Summary statistics
                    fluidRow(
                        column(12,
                            h3("Summary Statistics"),
                            verbatimTextOutput("statistics")
                        )
                    )
                ),
                
                # Add new Export tab
                tabPanel("Export Data",
                    fluidRow(
                        column(12,
                            h3("Export Crossing Plan and Cubicle Data"),
                            wellPanel(
                                textAreaInput("export_description",
                                            "Add Description/Notes:",
                                            rows = 4,
                                            placeholder = "Enter any additional notes or description about this crossing plan..."),
                                
                                selectInput("export_format",
                                          "Export Format:",
                                          choices = c("CSV" = "csv", 
                                                    "Excel" = "xlsx")),
                                
                                downloadButton("download_data", "Download Data"),
                                
                                hr(),
                                
                                # Preview section
                                h4("Data Preview"),
                                verbatimTextOutput("export_preview")
                            )
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
        
        # Add modal dialog for cubicle creation
        showModal(modalDialog(
            title = "Create New Cubicle",
            textInput("new_cubicle_id", "Cubicle ID", 
                     value = paste0("C", length(cubicles()) + 1)),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("confirm_cubicle", "Create")
            )
        ))
        
        # Store selected rows for use in the confirmation
        selected_crosses_reactive(crossing_plan()[selected_rows, ])
    })
    
    # Add new reactive value to store temporarily selected crosses
    selected_crosses_reactive <- reactiveVal(NULL)
    
    # Add confirmation handler
    observeEvent(input$confirm_cubicle, {
        selected_crosses <- selected_crosses_reactive()
        new_id <- input$new_cubicle_id
        
        # Validate ID is unique
        current_cubicles <- cubicles()
        if (any(sapply(current_cubicles, function(x) x$id == new_id))) {
            showNotification("This ID is already in use. Please choose another.", type = "error")
            return()
        }
        
        # Create new cubicle with user-specified ID
        unique_males <- unique(selected_crosses$male)
        new_cubicle <- list(
            id = new_id,
            male = unique_males[1],
            crosses = selected_crosses,
            notes = ""
        )
        
        # Update crossing plan status
        plan_data <- crossing_plan()
        selected_rows <- which(plan_data$female %in% selected_crosses$female & 
                              plan_data$male %in% selected_crosses$male)
        plan_data$status[selected_rows] <- "Assigned"
        plan_data$cubicle_id[selected_rows] <- new_id
        crossing_plan(plan_data)
        
        # Add new cubicle
        current_cubicles[[length(current_cubicles) + 1]] <- new_cubicle
        cubicles(current_cubicles)
        
        removeModal()
    })
    
    # Store cubicle data
    cubicles <- reactiveVal(list())
    
    # Render cubicle table
    output$cubicle_table <- renderDT({
        current_cubicles <- cubicles()
        
        if (length(current_cubicles) == 0) {
            return(NULL)
        }
        
        # Create data frame from cubicles
        cubicle_df <- do.call(rbind, lapply(current_cubicles, function(cubicle) {
            data.frame(
                Cubicle_ID = cubicle$id,
                Male = cubicle$male,
                Females = paste(cubicle$crosses$female, collapse = ", "),
                Planting_Date = format(input$planting_date, "%Y-%m-%d"),
                Expected_Flowering = format(input$expected_flowering, "%Y-%m-%d"),
                Notes = cubicle$notes,
                stringsAsFactors = FALSE
            )
        }))
        
        # Create editable datatable
        datatable(
            cubicle_df,
            editable = list(target = "cell", disable = list(columns = c(1, 2, 3, 4, 5))), # Only Notes column editable
            options = list(
                pageLength = 10,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
            ),
            selection = "single"
        )
    })
    
    # Add observer for note edits
    observeEvent(input$cubicle_table_cell_edit, {
        info <- input$cubicle_table_cell_edit
        current_cubicles <- cubicles()
        
        # Update notes in cubicle data
        if (info$col == 6) { # Notes column
            cubicle_id <- current_cubicles[[info$row]]$id
            current_cubicles[[info$row]]$notes <- info$value
            cubicles(current_cubicles)
        }
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
    
    # Add observer for ID changes
    observe({
        current_cubicles <- cubicles()
        
        lapply(current_cubicles, function(cubicle) {
            old_id <- cubicle$id
            new_id <- input[[paste0("cubicle_id_", old_id)]]
            
            if (!is.null(new_id) && new_id != old_id) {
                # Check if new ID is unique
                if (!any(sapply(current_cubicles, function(x) x$id == new_id))) {
                    # Update cubicle ID
                    cubicle$id <- new_id
                    
                    # Update crossing plan
                    plan_data <- crossing_plan()
                    plan_data$cubicle_id[plan_data$cubicle_id == old_id] <- new_id
                    crossing_plan(plan_data)
                    
                    # Update cubicles list
                    current_cubicles[[which(sapply(current_cubicles, function(x) x$id == old_id))]] <- cubicle
                    cubicles(current_cubicles)
                }
            }
        })
    })
    
    # Create formatted export data
    format_export_data <- reactive({
        req(crossing_plan())
        plan_data <- crossing_plan()
        cubicle_data <- cubicles()
        
        # Format crossing plan with cubicle assignments
        export_data <- plan_data
        
        # Create cubicle layout table
        cubicle_table <- do.call(rbind, lapply(cubicle_data, function(cubicle) {
            data.frame(
                Cubicle_ID = cubicle$id,
                Male = cubicle$male,
                Females = paste(cubicle$crosses$female, collapse = ", "),
                Planting_Date = format(input$planting_date, "%Y-%m-%d"),
                Expected_Flowering = format(input$expected_flowering, "%Y-%m-%d"),
                Notes = cubicle$notes,
                stringsAsFactors = FALSE
            )
        }))
        
        # Add description if provided
        attr(export_data, "description") <- input$export_description
        attr(export_data, "exported_at") <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        
        list(
            crossing_plan = export_data,
            cubicle_layout = cubicle_table
        )
    })
    
    # Preview of export data
    output$export_preview <- renderPrint({
        export_data <- format_export_data()
        
        cat("Description:", input$export_description, "\n\n")
        cat("Export will include:\n")
        cat("- Crossing plan with", nrow(export_data$crossing_plan), "crosses\n")
        cat("- Cubicle assignments and status\n")
        if (!is.null(export_data$cubicle_notes)) {
            cat("- Notes for", nrow(export_data$cubicle_notes), "cubicles\n")
        }
        cat("\nExport format:", input$export_format, "\n")
    })
    
    # Download handler
    output$download_data <- downloadHandler(
        filename = function() {
            paste0("crossing_plan_export_", 
                   format(Sys.time(), "%Y%m%d"), 
                   ".", input$export_format)
        },
        content = function(file) {
            export_data <- format_export_data()
            
            if (input$export_format == "csv") {
                # Save description to separate text file
                desc_file <- sub("\\.csv$", "_description.txt", file)
                writeLines(c(
                    paste("Description:", input$export_description),
                    paste("Exported:", attr(export_data$crossing_plan, "exported_at"))
                ), desc_file)
                
                # Create a directory for the CSV files
                dir_name <- sub("\\.csv$", "_files", file)
                dir.create(dir_name, showWarnings = FALSE)
                
                # Save crossing plan
                crossing_file <- file.path(dir_name, "crossing_plan.csv")
                write.csv(export_data$crossing_plan, crossing_file, row.names = FALSE)
                
                # Save cubicle layout including notes - this is the main file
                write.csv(export_data$cubicle_layout, file, row.names = FALSE)
                
            } else if (input$export_format == "xlsx") {
                wb <- openxlsx::createWorkbook()
                
                # Add description sheet
                openxlsx::addWorksheet(wb, "Description")
                openxlsx::writeData(wb, "Description", 
                                  data.frame(
                                      Description = input$export_description,
                                      Exported = attr(export_data$crossing_plan, "exported_at")
                                  ))
                
                # Add crossing plan sheet
                openxlsx::addWorksheet(wb, "Crossing Plan")
                openxlsx::writeData(wb, "Crossing Plan", export_data$crossing_plan)
                
                # Add cubicle layout sheet
                openxlsx::addWorksheet(wb, "Cubicle Layout")
                openxlsx::writeData(wb, "Cubicle Layout", export_data$cubicle_layout)
                
                openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
            }
        }
    )
}

shinyApp(ui = ui, server = server)
