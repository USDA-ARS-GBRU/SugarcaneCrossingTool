#Crosses.R
crosses_server <- function(input, output, session, reactive_cid, inventory_init,clone_assignments) {
  
  
# Event reactive that triggers when the "makecrosses" input is clicked
crosses_init <- eventReactive(input$makecrosses, withProgress(message = "Pulling Cross Data", {
  
    # Filter the inventory data to get unique clones
    germplasm <- as.data.frame(inventory_init())
    germplasm<-germplasm[duplicated(germplasm$Clone)==FALSE,]
    
    #Only show sorted clones
    display<-c(input$male_list,input$female_list)
    display_male<-c(input$male_list)
    display_female<-c(input$female_list)
    germplasm<-germplasm[which(germplasm$Clone%in%display),]

    # Get historical cross table using the InitCrossTable function
    historical_cross_table <- InitCrossTable(cross_list = historical_crosses, new_crosses=F, germplasm=germplasm)
    
    #Get new crosses table using the InitCrossTable function and data from ba_crosses_study
    new_crosses_table <- InitCrossTable(
      cross_list = ba_crosses_study(con = brap2, crossingProjectDbId = reactive_cid(), rclass = "data.frame"),
      Female.Parent = "data.parent1.germplasmName", Male.Parent = "data.parent2.germplasmName", new_crosses = T, germplasm=germplasm
    )

    # Combine the historical and new crosses tables
    all_cross_table <- rbind(historical_cross_table, new_crosses_table)

    #all_cross_table<-historical_cross_table
    
    # Remove rownames
    rownames(all_cross_table) <- NULL
    
   all_cross_table<-all_cross_table[which(all_cross_table$Female.Parent%in%display_female)&which(all_cross_table$Male.Parent%in%display_male),]   
    
    # Order the combined cross table based on the second and first columns
    
    return(all_cross_table[order(all_cross_table[, 2], all_cross_table[, 1]), ])
    
    
    
    }))

  # Render the crosses table using DT package
  output$crossesTable <- ({
    renderDT(crosses_init(), extensions = "FixedColumns", options = list(
      scrollX = TRUE, fixedColumns = list(leftColumns = 3)
    ))
  })
}
