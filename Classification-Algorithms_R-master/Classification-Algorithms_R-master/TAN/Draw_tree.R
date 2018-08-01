Draw_tree <- function(MST_adjacency){
  tree_adjacency = MST_adjacency + t(MST_adjacency) #make it symmetric
  n_row = nrow(tree_adjacency)
  PC_matrix = matrix(0, ncol=2, nrow = (n_row-1) )
  colnames(PC_matrix) = (c("Parent","Child"))
  parent = 1  #first column is always root
  parent_index = 1
  PC_index = 1
  while(PC_index <= nrow(PC_matrix) ){      #looping until fill up PC_matrix
    has.child = is.element(1,tree_adjacency[parent, ])
    if(has.child == TRUE){
      child = which(tree_adjacency[parent, ] == 1)
      PC_matrix[PC_index:(PC_index + (length(child)-1)), 1] = parent  #save parent, child in PC_matrix
      PC_matrix[PC_index:(PC_index + (length(child)-1)), 2] = child
      for(i in PC_index:(PC_index + (length(child)-1))){  #delete symmetric value
        tree_adjacency[PC_matrix[i,2],PC_matrix[i,1]] = 0
      }
      PC_index = PC_index + length(child)
    }
    parent = PC_matrix[parent_index, 2]  #child will be next parent
    parent_index = parent_index + 1
  }
  tree_struc = matrix(0, ncol = ncol(tree_adjacency), nrow = nrow(tree_adjacency))
  for(i in 1:nrow(PC_matrix)){
    parent = PC_matrix[i,1]
    child = PC_matrix[i,2]
    tree_struc[child,parent] = 1
  }
  return(tree_struc)
}

#tree_struc = Draw_tree(MST_adjacecy)
