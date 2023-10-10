## read in data (replace with your file path)
pedigree_download<-read.csv("data/demo_fullpedigree.csv") #needs to be updated each year
historical_crosses<-read.csv("data/demo_historicalcrosses.csv") #needs to be updated each year

studydbid<-"3654" #needs to be updated each year
crossingprojectdbid<-"291" #needs to be updated every year

## INIT DB CONNECTION ----------------------

brap <- brapi::as.ba_db(
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
  version = "v1"
)

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


# #run once
# 
# seedlots<-jsonlite::fromJSON(ba_seedlots_details(brap2, pageSize = 1000, page=0, rclass="list"))
# 
# seedlots[["result"]][["data"]][["additionalInfo"]]<-NULL
# seedlots[["result"]][["data"]][["externalReferences"]]<-NULL
# seedlots2<-as.data.frame(seedlots[["result"]][["data"]])
# 
# for(i in 1:15){ 
#   t<-jsonlite::fromJSON(ba_seedlots_details(brap2, pageSize = 1000, page=i, rclass="json"))
#                    t[["result"]][["data"]][["additionalInfo"]]<-NULL
#                    t[["result"]][["data"]][["externalReferences"]]<-NULL
#                    t2<-as.data.frame(t[["result"]][["data"]])                 
#                    
#   seedlots2<-rbind(seedlots2,t2)
# }
# 
# S<-read.table("Seedlots.txt")
# 

#basically, seedlots in brapi seem real weird? Let't put this off for now.
# seedlots2<-seedlots[,-which(colnames(seedlots)=="data.externalReferences")]
# seedlots2$Cross.Unique.ID<-gsub("SL-","", seedlots2$data.seedLotName)
# 
# crosses_withseedlots<-historical_crosses %>% left_join(seedlots2, by="Cross.Unique.ID")

#which(duplicated(historical_crosses$Cross.Unique.ID)) #==4? <- follow up on this
#which(duplicated(seedlots2$Cross.Unique.ID))






