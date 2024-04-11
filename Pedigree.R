#Pedigree.R

## Pedigree and Progeny ----
pedigree_server <- function(input, output, session, reactive_iid, selectedClone, inventory_init) {
  pedigree_init <- eventReactive(input$makepedigree, withProgress(message = "Pulling Progeny Data", {
    germplasm <- as.data.frame(inventory_init())
    germplasm <- germplasm[duplicated(germplasm$Clone) == FALSE, ]

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

    for (i in 1:dim(pedigree)[1]) {
      pedigree[i, 4] <-
        fromJSON(brapi::ba_germplasm_progeny(con = brap, germplasmDbId = as.character(pedigree[i, 2]), rclass = "json"))$metadata$pagination$totalCount
    }

    colnames(pedigree)[4] <- "Number.Progeny"

    pedigree[, -which(colnames(pedigree) == "data.germplasmDbId")]
  }))

  deeppedigree_init <- eventReactive(input$selectedClone, {
    germplasm <- as.data.frame(inventory_init())
    germplasm <- germplasm[duplicated(germplasm$Clone) == FALSE, ]

    tmp <- jsonlite::fromJSON(ba_germplasm_pedigree(con = brap2, germplasmDbId = as.character(germplasm[which(germplasm$Clone == input$selectedClone), 2]), rclass = "json"))$result$data

    print("Structure of tmp:")
    print(str(tmp))

    return(tmp)
  })

  pedmatrix_init <- eventReactive(input$makepedigree, {
    germplasm <<- as.data.frame(inventory_init())
    germplasm <- germplasm[duplicated(germplasm$Clone) == FALSE, ]

    mat <- PedMatrix(pedigree_download)

    if ("LCP85-0384" %in% germplasm$Clone) {
      axis <- germplasm$Clone
    } else {
      axis <- c(germplasm$Clone, "LCP85-0384")
    }

    mat2 <- round(mat[axis, axis], 2)
    mat2 <- as.data.frame(mat2)
    mat2$Clone <- rownames(mat2)
    mat2 <- mat2[, c(dim(mat2)[2], 1:dim(mat2)[2] - 1)]
    return(mat2)
  })

  output$pedigreeTable <- ({
    renderDT(merge(pedigree_init(), pedmatrix_init()[, c("LCP85-0384", "Clone")],
      by = "Clone"
    ) %>% rename(Rel.2.LCP850384 = "LCP85-0384"), options = list(language = list(
      zeroRecords = "There are no pedigree records to display. Double check that there are inventory records for the date you selected"
    )))
  })

  selectedClone <- reactiveVal()

  output$cloneDropdown <- renderUI({
    selectInput("selectedClone", "Select a Clone", choices = unique(pedigree_init()$Clone))
  })

  output$pedigreeGraph <- renderVisNetwork({
    req(input$selectedClone)

    pedigree_data_val <<- deeppedigree_init()

    if (!is.null(pedigree_data_val) && nrow(pedigree_data_val) > 0) {
      graph <- createPedigreeGraph(pedigree_data_val)

      if (!is.null(graph)) {
        return(graph)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })

  output$pedigreeMatrix <- renderPlotly({
    pedmatrix_data <- pedmatrix_init()
    heatmaply(pedmatrix_data)
  })
}