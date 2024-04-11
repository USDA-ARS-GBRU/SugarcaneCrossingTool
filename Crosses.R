#Crosses.R
crosses_server <- function(input, output, session, reactive_iid, reactive_cid) {
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
}
