TAN_ST <- function(dat, num.of.sample){
  library(igraph)
  o_data = dat[,]
  #shuffle data, because most data is ordered by class column
  o_data <- o_data[sample(nrow(o_data)),]
  
  #split data into sample & training
  sample <- o_data[1:num.of.sample, ]
  training <- o_data[(num.of.sample+1):nrow(o_data), ]
  
  n_row = nrow(o_data)
  n_col = ncol(o_data)
  right_answer = 0
  n_class <- length(unique(o_data[,n_col]))

  #build a tree structure and CPT using training data
  M.I_matrix <- Calculate_mi(training)
  MST_adjacency = MST(M.I_matrix)
  tree_struc = Draw_tree(MST_adjacency)
  CPT = CPT_builder(sample, training, tree_struc)
  #for each sample, calculate probability
  for(sample_i in 1:nrow(sample)){  
    this_sample = sample[sample_i, ]
    prob_table = matrix(0,nrow=n_class,ncol=(n_col+1))
    #for each column, find its parent, calculate conditional probability with parent's value.
    for(column_i in 1:(n_col-1)){
      #check if this column has parent
      has_parent = is.element(1,tree_struc[column_i, ])
      if(has_parent == FALSE){
        #No_parent -> check CPT[[n_col]]
        column_value = as.numeric(this_sample[column_i])
        prob_table[ ,column_i] = CPT[[n_col]][[column_i]][ ,(column_value+1)]
      }
      else{
        #Yes_parent -> get parent, value of parent and check CPT[[column_i]][[parent_value]]
        column_value = as.numeric(this_sample[column_i])
        parent = which(tree_struc[column_i, ]==1)
        parent_value = as.numeric(this_sample[parent])
        prob_table[ ,column_i] = CPT[[column_i]][[(parent_value+1)]][ ,(column_value+1)]
      }
    }
    
    #add class probability
    for(class_i in 1:n_class){
      count = length(which(training[,n_col] == (class_i-1)))
      if(count == 0){ count = 1 }
      prob_table[class_i, n_col] = count
    }
    prob_table[ ,n_col] = prob_table[ ,n_col]/nrow(training)
    
    prob_table[ ,(n_col+1)] = apply(prob_table[,1:n_col], 1, prod)
    guess = which(prob_table[,(n_col+1)] == max(prob_table[,(n_col+1)]))
    guess = guess -1
    if(guess == this_sample[,n_col]){
      right_answer = right_answer+1
    }
  }
  
  acc = right_answer/nrow(sample)
  return(acc)
}