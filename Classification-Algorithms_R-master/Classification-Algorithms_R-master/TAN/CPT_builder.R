CPT_builder <- function(testing, training, struc){
  tree_struc = struc[,]
  n_col = ncol(training)
  n_class = length(unique(training[,n_col]))
  #make a list of list CPT
  CPT <- list()
  for(i in 1:ncol(training)){
    CPT[[length(CPT)+1]] <- list(matrix(0,1,1))
  }
  
  #make the first CPT table with root-feature #root =without parents (class is the only condition)
  for(column_i in 1:(n_col-1)){
    root = matrix(0, ncol =(max(training[,column_i],testing[,column_i])+1), nrow=(max(training[,n_col],testing[,n_col])+1))
    for(class_i in 1:nrow(root)){       #row and col fill up cpt table
      for(cate_i in 1:ncol(root)){
        count = length( which(training[ ,column_i] == (cate_i-1) & training[ ,n_col] == (class_i-1)) )
        if(count == 0){count = 1}
        base = length( which(training[,n_col] == (class_i-1)) )
        if(base == 0){base = nrow(training)}
        root[class_i,cate_i] = count/base
      }
    }
    CPT[[ncol(training)]][[column_i]] = root
  }

  for(child_i in 1:(n_col-1)){          #make a cpt table for each feature
    has.parent = is.element(1,tree_struc[child_i,])
    if(has.parent == TRUE){           #decide if it needs CPT or not checking parents from tree_struc
      parent = which(tree_struc[child_i,] == 1)
      max_parent = max(training[,parent], testing[,parent])   #get maximum value from parent column for loop
      for(parent_value in 1:(max_parent+1)){                    #add 1 since value starts from 0
        #make an empty table with maximum value of child_column, class_column
        condition_table = matrix(0, ncol=(max(training[,child_i],testing[,child_i])+1), nrow=(max(training[,n_col],testing[,n_col])+1)) 
        condition_training = training[training[,parent]==(parent_value-1), ]     #select row with specific parent_value
        
        for(class_i in 1:nrow(condition_table)){       #row and col fill up cpt table
          base = length( which(condition_training[,n_col] == (class_i-1)) )
          if(base == 0){base = nrow(training)}
          for(cate_i in 1:ncol(condition_table)){
              count = length( which(condition_training[ ,child_i] == (cate_i-1) & condition_training[,n_col] == (class_i-1)) )
              if(count == 0){count = 1/round(nrow(training),0)}
              condition_table[class_i,cate_i] = count/base
          }
        }
        #declare list of list in a different way...check it on webpage...doesnt work!
        CPT[[child_i]][[parent_value]] = condition_table  #save cpt table in CPT_list
      }
    }
    else{                            
      #does not need to build cpt   (built in CPT[[1]] alraedy)
    }
  }
  return (CPT)                
}

#CPT = CPT_builder(dat,dat,tree_struc)
