
# create pedigree matrix

PedMatrix <- function(pedigree) {
  ped <- pedigree[, 1:3]

  ## clean data

  # recode NAs and blank cells as 0
  ped[is.na(ped)] <- "0"
  # ped$Female_Parent<-gsub("^$","0", ped$Female_Parent)
  # ped$Male_Parent<-gsub("^$","0", ped$Male_Parent)

  # recode unknown accessions as 0
  ped$Accession <- gsub("unknown", "0", ped$Accession, fixed = T)
  ped$Male_Parent <- gsub("unknown", "0", ped$Male_Parent, fixed = T)
  ped$Female_Parent <- gsub("unknown", "0", ped$Female_Parent, fixed = T)

  # remove duplicate entries
  ped <- ped[!duplicated(ped$Accession), ]

  # get rid of unknown accessions in first column
  ped <- ped[-which(ped$Accession == 0), ]

  # convert characters to factors
  str(ped)
  ped <- as.data.frame(map_if(ped, is.character, as.factor))

  # calculate rel matrix
  relmat <- as.matrix(Amatrix(ped, ploidy = 10))

  return(relmat)
}



InitCrossTable <- function(cross_list, Female.Parent = "Female.Parent", Male.Parent = "Male.Parent", new_crosses = F) {
  if (dim(cross_list)[2] == 1) { # if cross list is empty (first day)
    return(NULL)
  }

  # new_crosses means that there won't be any progenies because the cross was made this year
  freq_crosses <- as.data.frame(table(cross_list[, Female.Parent], cross_list[, Male.Parent]))
  freq_crosses <- freq_crosses[-which(freq_crosses$Freq == 0), ] # get rid of crosses never made
  colnames(freq_crosses) <- c("Female.Parent", "Male.Parent", "Number.Crosses")

  if (new_crosses == F) {
    progeny_crosses <- as.data.frame(aggregate(Number.of.Progenies ~ Female.Parent + Male.Parent, FUN = sum, data = cross_list)) %>%
      rename(Progeny.Per.Cross = Number.of.Progenies)
    crosses_table <- freq_crosses %>% right_join(progeny_crosses, by = c("Female.Parent", "Male.Parent"))

    return(crosses_table)
  } else {
    freq_crosses$Progeny.Per.Cross <- "None yet, new cross this year"
    crosses_table <- freq_crosses
    return(crosses_table)
  }
}


createPedigreeGraph <- function(data) {

  
  if (nrow(data) > 0 && !data$pedigreeString[1]=="///") {
      
      # Create a data frame for nodes
      nodes <- data.frame(
        id = data$germplasmName,
        stringsAsFactors = FALSE)
      
      
      # Create a data frame for edges
      
      edges_male<-data.frame(matrix(NA, nrow=dim(data)[2]), ncol=5)
      edges_female<-data.frame(matrix(NA, nrow=dim(data)[2]), ncol=5)
      
    
      
      for(i in 1:dim(data)[1]){
        
        if (!dim(data[i,"parents"][[1]])[1]==0) {
          edges_female[i,1]<-data[i,"parents"][[1]]$germplasmName[1]
          edges_male[i,1]<-data[i,"parents"][[1]]$germplasmName[2]
          edges_female[i,2]<-edges_male[i,2]<-data[i, "germplasmName"]
          edges_female[i,3]<-edges_male[i,3]<-"to"
          edges_female[i,4]<-edges_male[i,4]<-FALSE
          edges_female[i,5]<-"red"
          edges_male[i,5]<-"blue"

        } 
        
      }
        edges<-rbind(edges_female, edges_male)
        colnames(edges)<-c("from", "to", "arrows", "stringsAsFactors", "color")
        edges<-edges[!is.na(edges$from),]
      
  
      # Create a visNetwork graph
      graph <- visNetwork(nodes, edges) %>%
        visNodes(shape = "dot", size = 20, font = list(size = 12)) %>%
        visEdges(arrows = "to") %>%
        visLayout(randomSeed = 123) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1))
      
     
      return(graph)
      
    } else {
      print("Error: No data or missing pedigree for the selected clone")
      return(NULL)
     } 
}




