# Flowering.R

flowering_server <- function(input, output, session, reactive_date, reactive_iid, dataSource) {
  inventory_init <<- eventReactive(input$brapipull, withProgress(message = "Pulling Inventory Data", {
    tryCatch({
      
      inven <- data.frame(brapi::ba_studies_table(con = brap, studyDbId = reactive_iid(), rclass="data.frame")) %>%
        filter(observationLevel == "plot") %>% # select just plant rows
        set_names(~(.)%>% str_replace_all("SUGARCANE.*","") %>% str_replace_all("\\.","")) %>% # take CO term out of colnames
        mutate_at('blockNumber', as.factor)
      
      inven$blockNumber<-revalue(inven$blockNumber, c("1"="West", "2"="East", "3"="Railcarts", "4"="Back"))
      
      inven_male<-filter(inven, grepl(reactive_date(),TasselCountMale)) %>% 
        select(germplasmName, blockNumber, notes, germplasmDbId, TasselCountMale) %>% 
        separate(TasselCountMale, into=c("Count",NA), sep=",") %>%
        group_by(germplasmName)
      
      male<-merge(aggregate(as.numeric(Count)~germplasmName+germplasmDbId,inven_male, sum ),
                  aggregate(blockNumber~germplasmName+germplasmDbId,inven_male, function(x) paste(unique(x), collapse=":")))
      
      inven_female<-filter(inven, grepl(reactive_date(),TasselCountFemale)) %>% 
        select(germplasmName, blockNumber, germplasmDbId, notes, TasselCountFemale) %>% 
        separate(TasselCountFemale, into=c("Count",NA), sep=",") %>%
        group_by(germplasmName)
      
      female<-merge(aggregate(as.numeric(Count)~germplasmName+germplasmDbId,inven_female, sum ),
                    aggregate(blockNumber~germplasmName+germplasmDbId,inven_female, function(x) paste(unique(x), collapse=":")))
      
      colnames(male)<-colnames(female)<-c("Clone", "germplasmDbId", "FlowerCount", "Location")
      
      inven2<-list(male, female)
      names(inven2)<-c("male", "female")
      
      dataSource("Data pulled from BrAPI")
      inven2
    }, error = function(e) {
      dataSource("Saved data is being rendered")
      data.frame(Clone = character(), germplasmDbId=character(), FloweringCount = numeric(), Location = character()) # Return an empty data frame with the expected columns
    })
  }))
  
  
  output$inventoryTableMale <- ({
    renderDT(inventory_init()$male, options = list(language = list(
      zeroRecords = "There are no records to display. Double check the date you selected and try again. 
      You may need to wait a few minutes if inventory records were recently uploaded"
    )))
  })
  
  output$inventoryTableFemale <- ({
    renderDT(inventory_init()$female, options = list(language = list(
      zeroRecords = "There are no records to display. Double check the date you selected and try again. 
      You may need to wait a few minutes if inventory records were recently uploaded"
    )))
  })
  
  # Export the inventory_init function
  return(inventory_init)
}