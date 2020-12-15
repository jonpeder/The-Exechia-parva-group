## Function: ancestor_traits, Jon Peder Lindemann, 30.07.2020
# In a given tree, find most recent common ancestors of a group of taxa sharing a given trait.
# Returns node-labels associated with trait-variable as type data.frame
# Input variables:
# x = tree
# y = vecor of a trait variable  (e.g. zoogeographical region)
# z = vector of species names matching the tree tip-labels

ancestor_traits <- function(x = NULL, y = NULL, z = NULL) {
  library(ape)
  library(ggtree)
  # Assign input variables to two new variables
  in_tree <- x
  in_table <- data.frame(y)
  rownames(in_table) <- z
  # Loop over each tree-tip to obtain internal nodes for each taxa. Output is a list of taxa with their associated internal loop.
  node.list <- list()
  for (i in 1:nrow(in_table)) {
    offspr <- sapply(rownames(in_table)[i], grep, in_tree$tip.label) # Find edge number of a given tip
    ances <- in_tree$edge[in_tree$edge[,2]==offspr][1] # Find ancester-edge-number of the tip-edge-number
    temp <- NA
    temp[1] <- offspr
    # For each tip, loop over each ancestral node until hitting the root. Add each node to a list.
    for (j in 1:100) {
      ances <- in_tree$edge[in_tree$edge[,2]==offspr][1]
      if (is.na(ances)) {
        break
      } else {
        offspr <- ances
        temp[j+1] <- ances
      }
    }
    node.list[[rownames(in_table)[i]]] <- temp # Output of loop. A list of tips with their internal nodes chronologically oredered.
  }
  
  # Loop over tree-tips and the internal nodes of each tree-tip (internal loop). Output is a dataframe of node-labels associated with trait-variables.
  p <- ggtree(in_tree) # draw the tree to make get_taxa_name function work
  out1 <- data.frame("node" = NA, "trait" = NA) # Output variable
  index <- 1
  for (i in 1:length(node.list)) {
    nodes <- node.list[[i]]
    # Loop over internal nodes of each tree tip
    for (j in 1:(length(nodes)-1)) {
      taxa <- get_taxa_name(p, nodes[j+1]) # Find taxa related to the given node
      # in_tree wether the given taxa share the same trait
      if (length(unique(in_table[taxa,])) != 1) {
        # If only one taxa is returned, taxa = character(0)
        if (identical(taxa <- get_taxa_name(p, nodes[j]), character(0))) {
          tips <- in_tree$tip.label[in_tree$edge[in_tree$edge== nodes[1]]] # Get tip-labels for the target clade
          trait <- as.character(in_table[tips,]) # Get traits for the target clade
          out1[index, ]<- c(nodes[1], trait) # Add traits and ancestral node for the target clade
          index <- index + 1
          break
          # When more than one taxa are returned 
        } else {
          tips <- get_taxa_name(p, nodes[j]) # Get tip-labels for the target clade
          trait <- as.character(in_table[tips[1],]) # Get traits for the target clade
          out1[index,] <- c(nodes[j], trait) # Add traits and ancestral node for the target clade
          index <- index + 1
          break
        }
      } else {
        next
      }
    }
    
  }
  # Remove doubble occurances
  out2 <- unique(out1)
  rownames(out2) <- 1:nrow(out2)
  out2$node <- as.numeric(out2$node)
  
  # Return output
  return(out2)
}