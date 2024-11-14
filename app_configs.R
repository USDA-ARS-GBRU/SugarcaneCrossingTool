## read in data (replace with your file path)
pedigree_download<-read.csv("data/demo_fullpedigree.csv") #needs to be updated each year
historical_crosses<-read.csv("data/demo_historicalcrosses.csv") #needs to be updated each year

URL="sugarcanebase.breedinginsight.net"
USERNAME="appuser"
PASS="appuser123"
## INIT DB CONNECTION ----------------------

location_iid_map <- list(
  "Florida" = "3654",  #needs to be updated each year
  "Louisiana" = "3678"  #needs to be updated each year
)

crosses_iid_map<-list(
  "DemoBreeder"="219"  #needs to be updated each year
)

brap <- brapi::as.ba_db(
  secure = FALSE,
  protocol = "https://",
  db =URL,
  port = 80,
  apipath = NULL,
  multicrop = FALSE,
  crop = "",
  user = USERNAME,
  password = PASS,
  token = "",
  granttype = "password",
  clientid = "rbrapi",
  bms = FALSE,
  version = "v1"
)

brap2 <- brapi::as.ba_db(
  secure = FALSE,
  protocol = "https://",
  db =  URL,
  port = 80,
  apipath = NULL,
  multicrop = FALSE,
  crop = "",
  user = USERNAME,
  password = PASS,
  token = "",
  granttype = "password",
  clientid = "rbrapi",
  bms = FALSE,
  version = "v2"
)

blups<-read.csv("data/StageWiseParentBLUPS.csv")
colnames(blups)[1]<-"Clone"

amat<-read.csv("data/ParentAmatrix.csv", row.names = 1, check.names = F)

