## read in data (replace with your file path)
pedigree_download<-read.csv("data/2024ParentPedigree.csv") #needs to be updated each year
historical_crosses<-read.csv("data/demo_historicalcrosses.csv") #needs to be updated each year

blup_data<-read.csv("data/StageWiseParentBLUPS.csv")
colnames(blup_data)[1]<-"Clone"

parent_amat<-read.csv("data/ParentAmatrix.csv", row.names=1, check.names=F)

## INIT DB CONNECTION ----------------------

location_iid_map <- list(
  "Florida" = "3687",  #needs to be updated each year
  "Louisiana" = "3678"  #needs to be updated each year
)

crosses_iid_map<-list(
  "Aliya"="219"  #needs to be updated each year
)

brap <- brapi::as.ba_db(
  secure = FALSE,
  protocol = "https://",
  db ="sugarcanebase.breedinginsight.net",
  port = 80,
  apipath = NULL,
  multicrop = FALSE,
  crop = "",
  user = "appuser",
  password = "appuser123",
  token = "",
  granttype = "password",
  clientid = "rbrapi",
  bms = FALSE,
  version = "v1"
)

brap2 <- brapi::as.ba_db(
  secure = FALSE,
  protocol = "https://",
  db =  "sugarcanebase.breedinginsight.net",
  port = 80,
  apipath = NULL,
  multicrop = FALSE,
  crop = "",
  user = "appuser",
  password = "appuser123",
  token = "",
  granttype = "password",
  clientid = "rbrapi",
  bms = FALSE,
  version = "v2"
)


