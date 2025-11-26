## read in data (replace with your file path)
pedigree_download<-read.csv("data/2025ParentPedigree.csv") #needs to be updated each year

historical_crosses<-read.csv("data/HistoricCrossEntries.csv") #needs to be updated each year

blup_data<-read.csv("data/StageWiseParentBLUPS.csv")
colnames(blup_data)[1]<-"Clone"

full_amat<-PedMatrix(read.csv("data/2025ParentPedigree_Full.csv"))
parent_amat<-full_amat[rownames(full_amat)%in%pedigree_download$Accession, colnames(full_amat)%in%pedigree_download$Accession]

## INIT DB CONNECTION ----------------------

location_iid_map <- list(
  "Florida" = "3758"
)

crosses_iid_map<-list(
  "Aliya"="3756"  #needs to be updated each year
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

