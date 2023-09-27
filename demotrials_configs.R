
## read in data (replace with your file path)
pedigree_download<-read.csv("crossingdemo_fullped.csv") #needs to be updated each year
historical_crosses<-read.csv("cross_entries_2023-09-26.csv")

studydbid<-"3654" #needs to be updated each year
crossingprojectdbid<-"291" #needs to be updated every year


#run once

# seedlots<-as.data.frame(ba_seedlots_details(brap2, pageSize = 2000, page=0, rclass="data.frame"))
# 
# for(i in 1:13){
#   t<-as.data.frame(ba_seedlots_details(brap2, pageSize = 2000, page=i, rclass="data.frame"))
#   seedlots<-rbind(seedlots,t)
# }


#basically, seedlots in brapi seem real weird? Let't put this off for now.
# seedlots2<-seedlots[,-which(colnames(seedlots)=="data.externalReferences")]
# seedlots2$Cross.Unique.ID<-gsub("SL-","", seedlots2$data.seedLotName)
# 
# crosses_withseedlots<-historical_crosses %>% left_join(seedlots2, by="Cross.Unique.ID")

#which(duplicated(historical_crosses$Cross.Unique.ID)) #==4? <- follow up on this
#which(duplicated(seedlots2$Cross.Unique.ID))






