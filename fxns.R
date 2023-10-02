
# create pedigree matrix

PEDMATRIX <- function(pedigree) {
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



init_cross_table <- function(cross_list, Female.Parent = "Female.Parent", Male.Parent = "Male.Parent", new_crosses = F) {
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

