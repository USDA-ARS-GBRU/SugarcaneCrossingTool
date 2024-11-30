# Flowering.R

flowering_server <- function(input, output, session, reactive_date, reactive_iid, dataSource) {
  inventory_init <- eventReactive(input$brapipull, {
    withProgress(message = "Pulling Inventory Data", {
      tryCatch({
        # Validate inputs
        req(reactive_date(), reactive_iid())
        
        inven <- data.frame(brapi::ba_studies_table(con = brap, studyDbId = reactive_iid(), rclass="data.frame")) %>%
          filter(observationLevel == "plant") %>%
          set_names(~(.)%>% str_replace_all("SUGARCANE.*","") %>% str_replace_all("\\.","")) %>%
          filter(FloweringTime == reactive_date()) %>%
          group_by(germplasmName, germplasmDbId) %>%
          summarise(count = n()) %>%
          rename(Clone = germplasmName, FloweringCount = count)
        
        if(nrow(inven) == 0) {
          showNotification("No flowering data found for selected date", type = "warning")
        }
        
        dataSource("Data pulled from BrAPI")
        inven
        
      }, error = function(e) {
        showNotification(paste("Error pulling inventory:", e$message), type = "error", duration = NULL)
        dataSource("Error occurred - using empty dataset")
        data.frame(Clone = character(), FloweringCount = numeric(), stringsAsFactors = FALSE)
      }, warning = function(w) {
        showNotification(paste("Warning:", w$message), type = "warning")
      })
    })
  })
  
  output$inventoryTable <- renderDT({
    tryCatch({
      req(inventory_init())
      inventory_init()[,-which(colnames(inventory_init())=="germplasmDbId")]
    }, error = function(e) {
      showNotification("Error displaying inventory table", type = "error")
      data.frame()
    })
  }, options = list(language = list(
    zeroRecords = "There are no records to display. Double check the date you selected and try again."
  )))
  
  return(inventory_init)
}