# Flowering.R

flowering_server <- function(input, output, session, reactive_date, reactive_iid, dataSource) {
  inventory_init <- eventReactive(input$brapipull, withProgress(message = "Pulling Inventory Data", {
    tryCatch({
      inven <- data.frame(brapi::ba_studies_table(con = brap, studyDbId = reactive_iid(), rclass="data.frame")) %>%
        filter(observationLevel == "plant") %>% # select just plant rows
        set_names(~(.)%>% str_replace_all("SUGARCANE.*","") %>% str_replace_all("\\.","")) %>%  # take CO term out of colnames
        filter(FloweringTime== reactive_date()) %>% 
        select(germplasmName, germplasmDbId, SexMFWM) %>% 
        group_by(germplasmName, germplasmDbId, SexMFWM) %>% 
        summarise(count = n()) %>%
        rename(Clone = germplasmName, FloweringCount = count, Sex = SexMFWM)
      dataSource("Data pulled from BrAPI")
      inven
    }, error = function(e) {
      dataSource("Saved data is being rendered")
      data.frame(Clone = character(), FloweringCount = numeric(), Sex = character()) # Return an empty data frame with the expected columns
    })
  }))
  
  output$inventoryTable <- ({
    renderDT(inventory_init()[,-which(colnames(inventory_init())=="germplasmDbId")], options = list(language = list(
      zeroRecords = "There are no records to display. Double check the date you selected and try again. 
      You may need to wait a few minutes if inventory records were recently uploaded"
    )))
  })
  
  # Export the inventory_init function
  return(inventory_init)
}