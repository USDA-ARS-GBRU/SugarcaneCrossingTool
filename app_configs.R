## read in data (replace with your file path)
pedigree_download<-read.csv("data/demo_fullpedigree.csv") #needs to be updated each year
historical_crosses<-read.csv("data/demo_historicalcrosses.csv") #needs to be updated each year

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



#For debugging --> 

inven <- data.frame(brapi::ba_studies_table(con = brap, studyDbId = "3654", rclass="data.frame")) %>%
  filter(observationLevel == "plant") %>% # select just plant rows
  set_names(~(.)%>% str_replace_all("SUGARCANE.*","") %>% str_replace_all("\\.","")) %>%
  filter(FloweringTime=="2023-10-10") %>%
  #filter(FloweringTime== reactive_date()) %>%
  select(germplasmName, germplasmDbId, SexMFWM) %>%
  group_by(germplasmName, germplasmDbId, SexMFWM) %>%
  summarise(count = n()) %>%
  rename(Clone = germplasmName, FloweringCount = count, Sex = SexMFWM)


#run once

#  seedlots<-jsonlite::fromJSON(ba_seedlots_details(brap3, pageSize = 1000, page=0, rclass="json"))
# 
# seedlots[["result"]][["data"]][["additionalInfo"]]<-NULL
# seedlots[["result"]][["data"]][["externalReferences"]]<-NULL
# seedlots2<-as.data.frame(seedlots[["result"]][["data"]])
# 
# for(i in 1:15){
#   t<-jsonlite::fromJSON(ba_seedlots_details(brap3, pageSize = 1000, page=i, rclass="json"))
#                    t[["result"]][["data"]][["additionalInfo"]]<-NULL
#                    t[["result"]][["data"]][["externalReferences"]]<-NULL
#                    t2<-as.data.frame(t[["result"]][["data"]])
# 
#   seedlots2<-rbind(seedlots2,t2)
# }

# S<-read.table("Seedlots.txt")
#

#basically, seedlots in brapi seem real weird? Let't put this off for now.
# seedlots2<-seedlots[,-which(colnames(seedlots)=="data.externalReferences")]
# seedlots2$Cross.Unique.ID<-gsub("SL-","", seedlots2$data.seedLotName)
#
# crosses_withseedlots<-historical_crosses %>% left_join(seedlots2, by="Cross.Unique.ID")

#which(duplicated(historical_crosses$Cross.Unique.ID)) #==4? <- follow up on this
#which(duplicated(seedlots2$Cross.Unique.ID))