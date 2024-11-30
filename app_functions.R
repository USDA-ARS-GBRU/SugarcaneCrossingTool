# create pedigree matrix
PedMatrix <- function(pedigree) {
  tryCatch({
    if (is.null(pedigree) || nrow(pedigree) == 0) {
      return(matrix(nrow = 0, ncol = 0))
    }
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
  }, error = function(e) {
    warning("Error creating pedigree matrix:", e$message)
    return(matrix(nrow = 0, ncol = 0))
  })
}

InitCrossTable <- function(cross_list, Cross.Name="Cross.Unique.ID", Female.Parent = "Female.Parent", Male.Parent = "Male.Parent", new_crosses = F, germplasm) {
  tryCatch({
    # Input validation
    if(is.null(cross_list) || !is.data.frame(cross_list)) {
      warning("cross_list is NULL or not a data frame")
      return(data.frame())
    }
    
    if(ncol(cross_list) < 2) {
      warning("cross_list has insufficient columns")
      return(data.frame())
    }
    
    #filter first! goes faster
    cross_list2 <- cross_list[which(cross_list[[Female.Parent]] %in% germplasm$Clone & 
                                   cross_list[[Male.Parent]] %in% germplasm$Clone), ]
    
    if(nrow(cross_list2) == 0) {
      warning("No matching crosses found after filtering")
      return(data.frame())
    }
    
    if(new_crosses == FALSE) {
      ### get seedlot information
      tryCatch({
        cross_names <- gsub("^crossName=|&$", "", 
                           paste(paste0("crossName=",
                                      cross_list2[[Cross.Name]], "&"),
                                 collapse=""))
        
        seeds <- brapi::ba_seedlots_details(
          con = brap2, 
          crossName = cross_names, 
          rclass = "data.frame"
        )
        
        if(!is.data.frame(seeds) || nrow(seeds) == 0) {
          warning("No seedlot data returned")
          return(data.frame())
        }
        
        seeds$data.amount <- as.numeric(as.character(seeds$data.amount))
        seeds[[Cross.Name]] <- gsub("SL-", "", seeds$data.seedLotName)
        
        #join with subset cross list
        cross_list2 <- cross_list2 %>% 
          left_join(seeds, by = Cross.Name)
        
      }, error = function(e) {
        warning(paste("Error getting seedlot details:", e$message))
        return(data.frame())
      })
    }
    
    return(cross_list2)
    
  }, error = function(e) {
    warning(paste("Error in InitCrossTable:", e$message))
    return(data.frame())
  })
}

createPedigreeGraph <- function(data, selected_clone_id = NULL) {
  tryCatch({
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
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
  }, error = function(e) {
    warning("Error creating pedigree graph:", e$message)
    return(NULL)
  })
}

optimize_crosses <- function(inventory_data, male_parents, female_parents, n_crosses, max_crosses_per_parent, culling_k, prop_sel, blup, amat, weights) {
  tryCatch({
    # Validate inputs
    if (length(male_parents) == 0 || length(female_parents) == 0) {
      stop("No parents selected")
    }
    
    if (abs(sum(weights) - 1) > 0.01) {
      stop("Trait weights must sum to 1")
    }
    
    # Filter inventory data
    selected_parents <- c(male_parents, female_parents)
    filtered_inventory <- inventory_data[inventory_data$Clone %in% selected_parents, ]
    
    if (nrow(filtered_inventory) == 0) {
      stop("No valid inventory data for selected parents")
    }
    
    # Filter BLUP data
    blup <- blup[blup$Clone %in% selected_parents, ]
    if (nrow(blup) == 0) {
      stop("No BLUP data available for selected parents")
    }
    
    # Filter relationship matrix
    amat <- tryCatch({
      as.matrix(amat[selected_parents, selected_parents])
    }, error = function(e) {
      stop("Error processing relationship matrix: ", e$message)
    })
    
    # Create crossing plan
    cross_plan <- SimpleMating::planCross(TargetPop = female_parents, TargetPop2 = male_parents)
    
    # Debug print
    print("Cross plan:")
    print(head(cross_plan))
    print(paste("Number of crosses:", nrow(cross_plan)))
    
    # Predict mid-parent average
    mpa <- tryCatch({
      SimpleMating::getMPA(MatePlan = cross_plan,
                          Criterion = blup[,1:4],
                          K = amat,
                          Weights = weights)
    }, error = function(e) {
      print(paste("Error in MPA calculation:", e$message))
      return(NULL)
    })
    
    if (is.null(mpa)) {
      return(list(crosses = data.frame(Message = "Error in MPA calculation"), plot = NULL))
    }
    
    # Select crosses
    optimized_plan <- tryCatch({
      plan <- SimpleMating::selectCrosses(data = mpa,
                                        n.cross = n_crosses,
                                        max.cross = max_crosses_per_parent,
                                        min.cross = 1,
                                        culling.pairwise.k = culling_k)
      
      if (is.null(plan) || length(plan) < 2 || is.null(plan[[2]])) {
        return(NULL)
      }
      
      # Rename columns and round Y and K values
      plan[[2]] <- plan[[2]] %>%
        rename(Female.Parent = Parent1, Male.Parent = Parent2) %>%
        mutate(across(c(Y, K), ~round(., 3)))
      
      plan
      
    }, error = function(e) {
      print(paste("Error in cross selection:", e$message))
      return(NULL)
    })
    
    if (is.null(optimized_plan)) {
      return(list(
        crosses = data.frame(
          Message = "No valid crosses found. Try adjusting the culling parameter or increasing the number of parents."
        ), 
        plot = NULL
      ))
    }
    
    # Prepare output
    crosses <- optimized_plan[[2]]
    plot <- optimized_plan[[3]]
    
    return(list(crosses = crosses, plot = plot))
  }, error = function(e) {
    # Return informative error message
    return(list(
      crosses = data.frame(Message = paste("Error:", e$message)),
      plot = NULL
    ))
  }, warning = function(w) {
    # Log warning but continue
    warning(w$message)
  })
}

fetch_pedigree_data <- function(clone) {
  # This is a placeholder implementation. You should replace this with actual data fetching logic.
  # For example, you might query a database or use BrAPI calls to get the pedigree information.
  
  # For demonstration purposes, let's create some dummy data
  parents <- c(paste0(clone, "_Parent1"), paste0(clone, "_Parent2"))
  grandparents <- c(paste0(parents[1], "_Parent1"), paste0(parents[1], "_Parent2"),
                    paste0(parents[2], "_Parent1"), paste0(parents[2], "_Parent2"))
  
  pedigree_data <- data.frame(
    parent = c(parents, grandparents),
    child = c(rep(clone, 2), rep(parents, each = 2))
  )
  
  return(pedigree_data)
}