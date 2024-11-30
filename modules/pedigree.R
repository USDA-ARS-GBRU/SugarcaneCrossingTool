#Pedigree.R

## Pedigree and Progeny ----
pedigree_server <- function(input, output, session, reactive_iid, selectedClone, inventory_init, clone_assignments) {
  pedigree_init <- eventReactive(input$makepedigree, {
    withProgress(message = "Pulling Progeny Data", {
      tryCatch({
        # Validate inputs
        req(inventory_init())
        germplasm <- as.data.frame(inventory_init())
        if(nrow(germplasm) == 0) {
          stop("No inventory data available")
        }
        
        # Only show sorted clones
        display <- c(input$male_list, input$female_list)
        if(length(display) == 0) {
          stop("No clones sorted into male/female categories")
        }
        
        germplasm <- germplasm[which(germplasm$Clone %in% display),]
        
        tmp <- stripClass(
          as.data.frame(
            ba_germplasm_details2(con = brap2, germplasmQuery = as.character(paste0("?studyDbId=", reactive_iid(), "&pageSize=1000")), rclass = "data.frame")
          ),
          classString = "ba_germplasm_details"
        )

        pedigree <- tmp[tmp$data.germplasmName %in% germplasm$Clone, c("data.germplasmName", "data.germplasmDbId", "data.pedigree")] %>%
          rename(Clone = data.germplasmName, Pedigree = data.pedigree)

        pedigree <- tmp[tmp$data.germplasmName %in% germplasm$Clone, c("data.germplasmName", "data.germplasmDbId", "data.pedigree")] %>%
          rename(Clone = data.germplasmName, Pedigree = data.pedigree)

        #note: could rewrite ba_germplam_progeny to speed performance
        
        for (i in 1:dim(pedigree)[1]) {
          pedigree[i, 4] <-
            fromJSON(brapi::ba_germplasm_progeny(con = brap, germplasmDbId = as.character(pedigree[i, 2]), rclass = "json"))$metadata$pagination$totalCount
        }

        colnames(pedigree)[4] <- "Number.Progeny"
      
      
        pedigree<-pedigree[,-which(colnames(pedigree) == "data.germplasmDbId")]
        
        return(pedigree)
      }, error = function(e) {
        showNotification(paste("Error getting pedigree data:", e$message), type = "error", duration = NULL)
        return(data.frame(Clone = character(), Pedigree = character(), Number.Progeny = numeric()))
      }, warning = function(w) {
        showNotification(paste("Warning:", w$message), type = "warning")
      })
    })
  })

  deeppedigree_init <- eventReactive(input$selectedClone, {
    tryCatch({
      req(input$selectedClone, inventory_init())
      germplasm <- as.data.frame(inventory_init())
      germplasm <- germplasm[duplicated(germplasm$Clone) == FALSE, ]

      tmp <- jsonlite::fromJSON(ba_germplasm_pedigree(con = brap2, germplasmDbId = as.character(germplasm[which(germplasm$Clone == input$selectedClone), 2]), rclass = "json"))$result$data

      print("Structure of tmp:")
      print(str(tmp))

      return(tmp)
    }, error = function(e) {
      showNotification(paste("Error getting detailed pedigree:", e$message), type = "error")
      return(NULL)
    })
  })

  pedmatrix_init <- eventReactive(input$makepedigree, {
    tryCatch({
      req(input$male_list, input$female_list)
      germplasm <<- as.data.frame(inventory_init())
      germplasm <- germplasm[duplicated(germplasm$Clone) == FALSE, ]

      mat <- PedMatrix(pedigree_download)

      #get rid of this for now
      # if ("LCP85-0384" %in% germplasm$Clone) {
      #   axis <- germplasm$Clone
      # } else {
      #   axis <- c(germplasm$Clone, "LCP85-0384")
      # }
      
      mat2 <- round(mat[input$male_list,input$female_list ], 2)
      mat2 <- as.data.frame(mat2)
      mat2$Clone <- rownames(mat2)
      mat2 <- mat2[, c(dim(mat2)[2], 1:dim(mat2)[2] - 1)]
      return(mat2)
    }, error = function(e) {
      showNotification(paste("Error creating pedigree matrix:", e$message), type = "error")
      return(data.frame())
    })
  })

  #get rid of this for now
  # output$pedigreeTable <- ({
  #   renderDT(merge(pedigree_init(), pedmatrix_init()[, c("LCP85-0384", "Clone")],
  #     by = "Clone"
  #   ) %>% rename(Rel.2.LCP850384 = "LCP85-0384"), options = list(language = list(
  #     zeroRecords = "There are no pedigree records to display. Double check that there are inventory records for the date you selected"
  #   )))
    
    
    output$pedigreeTable <- renderDT({
      tryCatch({
        req(pedigree_init())
        pedigree_init()
      }, error = function(e) {
        data.frame()
      })
    }, options = list(language = list(
      zeroRecords = "No pedigree records available. Check inventory records for selected date."
    )))

  selectedClone <- reactiveVal()

  output$cloneDropdown <- renderUI({
    selectInput("selectedClone", "Select a Clone", choices = unique(pedigree_init()$Clone))
  })

  output$pedigreeGraph <- renderVisNetwork({
    tryCatch({
      req(input$selectedClone, deeppedigree_init())
      createPedigreeGraph(deeppedigree_init())
    }, error = function(e) {
      showNotification("Error displaying pedigree graph", type = "error")
      NULL
    })
  })

  output$pedigreeMatrix <- renderPlotly({
    tryCatch({
      req(pedmatrix_init())
      heatmaply(pedmatrix_init(), xlab="Female Parent", ylab="Male Parent")
    }, error = function(e) {
      showNotification("Error displaying pedigree matrix", type = "error")
      plotly_empty()
    })
  })
}