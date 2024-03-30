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
  if (dim(cross_list)[2] == 1) {
    # if cross list is empty (first day)
    return(NULL)
  }
  # new_crosses means that there won't be any progenies because the cross was made this year
  freq_crosses <- as.data.frame(table(cross_list[, Female.Parent], cross_list[, Male.Parent]))
  freq_crosses <- freq_crosses[-which(freq_crosses$Freq == 0), ] # get rid of crosses never made
  colnames(freq_crosses) <- c("Female.Parent", "Male.Parent", "Number.Crosses")
  if (new_crosses == F) {
    progeny_crosses <- as.data.frame(aggregate(Number.of.Progenies ~ Female.Parent + Male.Parent, FUN = sum, data = cross_list)) %>%
      rename(Progeny.Per.Cross = Number.of.Progenies)
    crosses_table <- freq_crosses %>%
      right_join(progeny_crosses, by = c("Female.Parent", "Male.Parent"))
    return(crosses_table)
  } else {
    freq_crosses$Progeny.Per.Cross <- "None yet, new cross this year"
    crosses_table <- freq_crosses
    return(crosses_table)
  }
}


createPedigreeGraph <- function(data, selected_clone_id = NULL) {
  if (!is.null(data) && nrow(data) > 0) {
    # Create a data frame for nodes
    nodes <- data.frame(
      id = data$germplasmDbId,
      label = data$germplasmName,
      color = ifelse(data$germplasmDbId %in% unique(unlist(lapply(data$parents, function(x) x$germplasmDbId))), "lightblue", "purple"),
      shape = "dot",
      stringsAsFactors = FALSE
    )
    
    # Remove rows with NA values from nodes
    nodes <- nodes[!is.na(nodes$id), ]
    
    # Check if selected_clone_id is provided and exists in nodes$id
    if (!is.null(selected_clone_id) && selected_clone_id %in% nodes$id) {
      # Color the selected clone node differently
      nodes$color[nodes$id == selected_clone_id] <- "orange"
    }
    
    # Create an empty data frame for edges
    edges <- data.frame(
      from = NULL,
      to = NULL,
      arrows = NULL,
      color = NULL,
      stringsAsFactors = FALSE
    )
    
    # Iterate over each row in the data
    for (i in 1:nrow(data)) {
      # Extract the parent information for the current germplasm
      parents <- data$parents[[i]]
      
      # Skip iteration if parents is an empty data frame
      if (is.data.frame(parents) && nrow(parents) == 0) {
        next
      }
      
      if (!is.null(parents) && nrow(parents) > 0) {
        # Filter out parents with missing germplasmDbId
        valid_parents <- parents[!is.na(parents$germplasmDbId) & parents$germplasmDbId != "", ]
        
        if (nrow(valid_parents) > 0) {
          # Add edges for each valid parent with colors based on parent type
          edges <- rbind(edges, data.frame(
            from = valid_parents$germplasmDbId,
            to = data$germplasmDbId[i],
            arrows = "to",
            color = ifelse(valid_parents$parentType == "MALE", "blue", "red"),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    # Remove rows with NA values from edges
    edges <- edges[!is.na(edges$from) & !is.na(edges$to), ]
    
    # Count the number of outgoing edges for each node
    node_contributions <- table(edges$from)
    
    # Update the size of nodes based on their contributions
    nodes$size <- 20  # Default size for nodes with no outgoing edges
    
    # Create a mapping of node IDs to their contributions
    node_contribution_map <- setNames(as.integer(node_contributions), names(node_contributions))
    
    # Update the size of nodes based on their contributions, handling NA values
    valid_node_ids <- intersect(nodes$id, names(node_contribution_map))
    nodes$size[nodes$id %in% valid_node_ids] <- 20 + 5 * node_contribution_map[valid_node_ids]
    
    # Create a visNetwork graph
    graph <- visNetwork(nodes, edges) %>%
      visNodes(
        shape = "dot",
        font = list(size = 12),
        color = list(background = "color", border = "black", highlight = "yellow"),
        size = nodes$size  # Set the node sizes directly in visNodes
      ) %>%
      visEdges(
        arrows = "to",
        color = list(color = "color", highlight = "red")
      ) %>%
      visHierarchicalLayout(direction = "UD", sortMethod = "directed") %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1))
    
    return(graph)
  } else {
    return(NULL)
  }
}