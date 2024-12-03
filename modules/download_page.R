#download_page.R

download_page_server <- function(input, output, session, reactive_date) {
  output$downloaddata <- downloadHandler(
    filename = function() {
      paste0(reactive_date(), "_STracT_data.xlsx")
    },
    content = function(file) {
      write_xlsx(list(
        CloneData = list(inventory_init(), merge(pedigree_init(), pedmatrix_init()[, c("LCP85-0384", "Clone")],
          by = "Clone"
        ), performance_init()) %>% reduce(full_join, by = "Clone"),
        RelationshipMatrix = pedmatrix_init(),
        PreviousCrosses = crosses_init()
      ), path = file)
    }
  )
}