#Crosses.R
crosses_server <- function(input, output, session, reactive_cid, inventory_init, clone_assignments, rv) {
  
  crosses_init <- eventReactive(input$makecrosses, {
    withProgress(message = "Pulling Cross Data", {
      tryCatch({
        # Validate inputs
        req(inventory_init(), reactive_cid())
        
        # Filter the inventory data to get unique clones
        germplasm <- as.data.frame(inventory_init())
        if(nrow(germplasm) == 0) {
          return(data.frame(
            Female.Parent = character(0),
            Male.Parent = character(0),
            stringsAsFactors = FALSE
          ))
        }
        
        germplasm <- germplasm[!duplicated(germplasm$Clone), ]
        
        # Validate sorted clones
        display <- c(input$male_list, input$female_list)
        display_male <- c(input$male_list)
        display_female <- c(input$female_list)
        
        if(length(display) == 0) {
          return(data.frame(
            Female.Parent = character(0),
            Male.Parent = character(0),
            stringsAsFactors = FALSE
          ))
        }
        
        germplasm <- germplasm[which(germplasm$Clone %in% display), ]
        
        # Get historical cross table
        historical_cross_table <- tryCatch({
          result <- InitCrossTable(cross_list = historical_crosses, new_crosses=F, germplasm=germplasm)
          if(is.null(result) || ncol(result) < 2) {
            data.frame(
              Female.Parent = character(0),
              Male.Parent = character(0),
              stringsAsFactors = FALSE
            )
          } else {
            result
          }
        }, error = function(e) {
          warning("Error getting historical crosses: ", e$message)
          data.frame(
            Female.Parent = character(0),
            Male.Parent = character(0),
            stringsAsFactors = FALSE
          )
        })
        
        # Get new crosses table
        new_crosses_table <- tryCatch({
          new_crosses <- ba_crosses_study(
            con = brap2, 
            crossingProjectDbId = reactive_cid(), 
            rclass = "data.frame"
          )
          
          if(is.null(new_crosses) || nrow(new_crosses) == 0) {
            data.frame(
              Female.Parent = character(0),
              Male.Parent = character(0),
              stringsAsFactors = FALSE
            )
          } else {
            result <- InitCrossTable(
              cross_list = new_crosses,
              Female.Parent = "data.parent1.germplasmName", 
              Male.Parent = "data.parent2.germplasmName", 
              new_crosses = T, 
              germplasm = germplasm
            )
            if(is.null(result) || ncol(result) < 2) {
              data.frame(
                Female.Parent = character(0),
                Male.Parent = character(0),
                stringsAsFactors = FALSE
              )
            } else {
              result
            }
          }
        }, error = function(e) {
          warning("Error getting new crosses: ", e$message)
          data.frame(
            Female.Parent = character(0),
            Male.Parent = character(0),
            stringsAsFactors = FALSE
          )
        })
        
        # Combine tables
        all_cross_table <- rbind(
          if(ncol(historical_cross_table) >= 2) historical_cross_table else NULL,
          if(ncol(new_crosses_table) >= 2) new_crosses_table else NULL
        )
        
        # If no data, return empty data frame with correct structure
        if(is.null(all_cross_table) || nrow(all_cross_table) == 0) {
          return(data.frame(
            Female.Parent = character(0),
            Male.Parent = character(0),
            stringsAsFactors = FALSE
          ))
        }
        
        # Remove rownames and filter
        rownames(all_cross_table) <- NULL
        all_cross_table <- all_cross_table[
          which(all_cross_table$Female.Parent %in% display_female) & 
            which(all_cross_table$Male.Parent %in% display_male),
        ]
        
        # Order the combined cross table
        all_cross_table <- all_cross_table[order(all_cross_table[, 2], all_cross_table[, 1]), ]
        
        # Store in reactiveValues
        rv$previous_crosses <- all_cross_table
        
        return(all_cross_table)
        
      }, error = function(e) {
        warning(paste("Error getting cross data:", e$message))
        return(data.frame(
          Female.Parent = character(0),
          Male.Parent = character(0),
          stringsAsFactors = FALSE
        ))
      })
    })
  })
  
  # Render the crosses table
  output$crossesTable <- renderDT({
    tryCatch({
      req(crosses_init())
      crosses_init()
    }, error = function(e) {
      showNotification("Error displaying crosses table", type = "error")
      data.frame()
    })
  }, extensions = "FixedColumns", options = list(
    scrollX = TRUE, 
    fixedColumns = list(leftColumns = 3),
    language = list(
      zeroRecords = "No crosses found. Check that you have selected parents and that cross data exists."
    )
  ))
}