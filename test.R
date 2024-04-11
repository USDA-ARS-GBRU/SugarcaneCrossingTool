library(brapi)
library(jsonlite)
library(dplyr)

brap2 <- brapi::as.ba_db(
  secure = FALSE,
  protocol = "https://",
  db = "sugarcanebase.breedinginsight.net",
  port = 80,
  apipath = NULL,
  multicrop = FALSE,
  crop = "",
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD"),
  token = "",
  granttype = "password",
  clientid = "rbrapi",
  bms = FALSE,
  version = "v2"
)

seedlots <- list()
seedlots[[1]] <- jsonlite::fromJSON(ba_seedlots_details(brap2, pageSize = 1000, page = 0, rclass = "json"))
seedlots[[1]]$result$data$additionalInfo <- NULL
seedlots[[1]]$result$data$externalReferences <- NULL
seedlots[[1]] <- seedlots[[1]]$result$data

for (i in 1:14) {
  seedlots[[i+1]] <- jsonlite::fromJSON(ba_seedlots_details(brap2, pageSize = 1000, page = i, rclass = "json"))
  seedlots[[i+1]]$result$data$additionalInfo <- NULL
  seedlots[[i+1]]$result$data$externalReferences <- NULL
  seedlots[[i+1]] <- seedlots[[i+1]]$result$data
}

seedlots2 <- bind_rows(seedlots)

# Remove duplicated seedLotDbId values
seedlots2 <- distinct(seedlots2, seedLotDbId, .keep_all = TRUE)

# Print the count of duplicated and non-duplicated seedLotDbId values
print(table(duplicated(seedlots2$seedLotDbId)))