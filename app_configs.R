## read in data (replace with your file path)
pedigree_download<-read.csv("data/demo_fullpedigree.csv") #needs to be updated each year
historical_crosses<-read.csv("data/demo_historicalcrosses.csv") #needs to be updated each year

## INIT DB CONNECTION ----------------------

location_iid_map <- list(
  "Florida" = "3654",  #needs to be updated each year
  "Louisiana" = "3678"  #needs to be updated each year
)

crosses_block_iid_map<-list(
  "DemoBreeder"="219"  #needs to be updated each year
)

brap <- brapi::as.ba_db(
  secure = FALSE,
  protocol = "https://",
  db =Sys.getenv("URL"),
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
  version = "v1"
)

brap2 <- brapi::as.ba_db(
  secure = FALSE,
  protocol = "https://",
  db =  Sys.getenv("URL"),
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



