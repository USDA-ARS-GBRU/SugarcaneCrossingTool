#Crosses.R
crosses_server <- function(input, output, session, reactive_cid, inventory_init, clone_assignments, rv) {
  
  crosses_init <- eventReactive(input$makecrosses, {
    withProgress(message = "Pulling Cross Data", {
      tryCatch({
        # Add diagnostic logging
        print("Available males:")
        print(unique(input$male_list))
        print("Available females:")
        print(unique(input$female_list))
        
        # Validate inputs
        req(inventory_init(), reactive_cid())
        
        # Filter the inventory data to get unique clones
        germplasm <- as.data.frame(rbind(inventory_init()$male, inventory_init()$female))
        germplasm <- germplasm[duplicated(germplasm$Clone)==FALSE,]

        # Only show sorted clones
        display <- c(unique(input$female_list), unique(input$male_list))
        display_male <- c(unique(input$male_list))
        display_female <- c(unique(input$female_list))
        
        print("Number of display clones:")
        print(length(display))
        print("Number of male clones:")
        print(length(display_male))
        print("Number of female clones:")
        print(length(display_female))
        
        germplasm <- germplasm[which(germplasm$Clone %in% display),]
        
        if(nrow(germplasm) == 0) {
          warning("No germplasm data after filtering")
          return(data.frame())
        }
        
        # Get historical cross table with more detailed error handling
        historical_cross_table <- tryCatch({
          print("Getting historical crosses...")
          result <- InitCrossTable(cross_list = historical_crosses, new_crosses=F, germplasm=germplasm)
          print("Number of historical crosses before filtering:")
          print(nrow(result))
          
          if(!is.null(result) && nrow(result) > 0) {
            # Filter crosses to only include sorted parents
            result <- result[result$Female.Parent %in% display_female & 
                           result$Male.Parent %in% display_male, ]
            print("Number of historical crosses after filtering:")
            print(nrow(result))
          }
          result
        }, error = function(e) {
          warning(paste("Error in historical crosses:", e$message))
          data.frame()
        })
        
        # Get new crosses table with more detailed error handling
        new_crosses_table <- tryCatch({
          print("Getting new crosses...")
          new_crosses <- ba_crosses_study(
            con = brap2, 
            crossingProjectDbId = reactive_cid(), 
            rclass = "data.frame"
          )
          
          if(!is.null(new_crosses) && nrow(new_crosses) > 0) {
            result <- InitCrossTable(
              cross_list = new_crosses,
              Female.Parent = "data.parent1.germplasmName", 
              Male.Parent = "data.parent2.germplasmName", 
              new_crosses = T, 
              germplasm = germplasm
            )
            print("Number of new crosses before filtering:")
            print(nrow(result))
            
            if(!is.null(result) && nrow(result) > 0) {
              # Filter crosses to only include sorted parents
              result <- result[result$Female.Parent %in% display_female & 
                             result$Male.Parent %in% display_male, ]
              print("Number of new crosses after filtering:")
              print(nrow(result))
            }
            result
          } else {
            data.frame()
          }
        }, error = function(e) {
          warning(paste("Error in new crosses:", e$message))
          data.frame()
        })
        
        # Combine tables with better error handling
        all_cross_table <- tryCatch({
          rbind(
            if(nrow(historical_cross_table) > 0) historical_cross_table else NULL,
            if(nrow(new_crosses_table) > 0) new_crosses_table else NULL
          )
        }, error = function(e) {
          warning(paste("Error combining cross tables:", e$message))
          data.frame()
        })
        
        if(is.null(all_cross_table) || nrow(all_cross_table) == 0) {
          warning("No crosses found after combining tables")
          return(data.frame())
        }
        
        # Store in reactiveValues
        rv$previous_crosses <- all_cross_table
        
        return(all_cross_table)
        
      }, error = function(e) {
        warning(paste("Error in crosses_init:", e$message))
        return(data.frame())
      })
    })
  })
  

recip_crosses_init <- eventReactive(input$makecrosses, {
    withProgress(message = "Pulling Cross Data", {
      tryCatch({
        # Validate inputs
        req(inventory_init(), reactive_cid())
        
        # Filter the inventory data to get unique clones
         germplasm <- as.data.frame(rbind(inventory_init()$male, inventory_init()$female))
    germplasm<-germplasm[duplicated(germplasm$Clone)==FALSE,]

    #Only show sorted clones
    display<-c(unique(input$female_list), unique(input$male_list))
    display_male<-c(unique(input$male_list))
    display_female<-c( unique(input$female_list))
   
    germplasm<-germplasm[which(germplasm$Clone%in%display),]
        if(nrow(germplasm) == 0) {
          return(data.frame(
            Female.Parent = character(0),
            Male.Parent = character(0),
            stringsAsFactors = FALSE
          ))
        }
        
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

          result <- InitCrossTable(cross_list = historical_crosses, new_crosses=F, Female.Parent="Male.Parent", Male.Parent="Female.Parent", germplasm=germplasm)
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
            result<- InitCrossTable(
    cross_list = ba_crosses_study(con = brap2, crossingProjectDbId = reactive_cid(), rclass = "data.frame"),
    Female.Parent = "data.parent2.germplasmName", Male.Parent = "data.parent1.germplasmName", new_crosses = T, germplasm=germplasm
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
         all_cross_table<-all_cross_table[intersect(which(all_cross_table$Female.Parent%in%display_male),which(all_cross_table$Male.Parent%in%display_female)),]

        
        # Order the combined cross table
        all_cross_table <- all_cross_table[order(all_cross_table[, 2], all_cross_table[, 1]), ]
        
        # Store in reactiveValues
        rv$recip_previous_crossess <- all_cross_table
        
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



  # Render the crosses table using DT package
  output$crossesTable <- ({
    renderDT( tryCatch({
      req(crosses_init())
      crosses_init()
    }, error = function(e) {
      showNotification("Error displaying crosses table", type = "error")
      data.frame()
    })
  , extensions = "FixedColumns", options = list(
      scrollX = TRUE, fixedColumns = list(leftColumns = 3)
    ))
  })


# Render the crosses table using DT package
output$recipCrossesTable <- ({
  renderDT(tryCatch({
      req(recip_crosses_init())
      recip_crosses_init()
    }, error = function(e) {
      showNotification("Error displaying crosses table", type = "error")
      data.frame()
  }, extensions = "FixedColumns", options = list(
    scrollX = TRUE, fixedColumns = list(leftColumns = 3)
  )))
})

}

                               

