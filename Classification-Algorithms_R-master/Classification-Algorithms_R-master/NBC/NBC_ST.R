NBC_ST <- function(dat,num.of.sample){
  o_data = dat[,]
  #shuffle data, because most data is ordered by class column
  o_data <- o_data[sample(nrow(o_data)),]
  #split data into sample & training
  sample_group <- o_data[1:num.of.sample, ]
  training_group <- o_data[(num.of.sample+1):nrow(o_data), ]
 
  #basic info
  n_row = nrow(o_data)
  n_col = ncol(o_data)
  right_answer = 0
  n_class <- length(unique(o_data[,n_col]))
  
  #build CPT with sampple and training group
  empty_table = matrix(0, nrow = (n_col-1), ncol=(n_col-1) )
  CPT = CPT_builder(sample_group, training_group, empty_table)
  
  #for each sample in sample group
  for(sample_index in 1:nrow(sample_group) ){
    this_sample = as.matrix(sample_group[sample_index, ])
    #build a empty prob_table
    prob_table = matrix(0,nrow=n_class,ncol=(n_col+1))
    #for each column, fill up prob table using CPT
    for(column_i in 1:(n_col-1)){
      column_value = as.numeric(this_sample[column_i])
      prob_table[ ,column_i] = CPT[[n_col]][[column_i]][ ,(column_value+1)]
    }
    #fill up priority probability 
    for(class_i in 1:n_class){
      count = length(which(training_group[,n_col] == (class_i-1)))
      if(count == 0){ count == 1 }
      prob_table[class_i, n_col] = count
    }
    prob_table[ ,n_col] = prob_table[ ,n_col]/nrow(training_group)
    #final answer, product each row
    prob_table[ ,(n_col+1)] = apply(prob_table[,1:n_col], 1, prod)
    guess = which(prob_table[,(n_col+1)] == max(prob_table[,(n_col+1)]))
    guess = guess - 1
    #check if guess is right answer or not
    if(guess == this_sample[,n_col]){
      right_answer = right_answer+1
    }
  }
  acc = right_answer / nrow(sample_group)
  return(acc)
}