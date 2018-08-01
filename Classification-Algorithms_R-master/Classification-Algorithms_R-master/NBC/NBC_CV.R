NBC_CV <- function(dat,fold){
  o_data <- dat[,]
  n_row = nrow(dat)
  n_col = ncol(dat)
  n_class <- length(unique(o_data[,n_col]))
  o_data <- o_data[sample(nrow(o_data)), ]     #shuffle data
  fold_data <- cut(seq(1,n_row),breaks=fold,labels=FALSE)
  right_answer = 0
  #loop by fold
  for(f in 1:fold){ 
    #outter loop , i = sample group index
    #split data into sample & training
    index <- which(fold_data==f,arr.ind=TRUE)
    sample_group <- o_data[index, ]
    training_group <- o_data[-index, ]
    empty_table = matrix(0, nrow = (n_col-1), ncol=(n_col-1) )
    #build CPT with empty_table
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
  }
  acc = right_answer / nrow(o_data)
  return(acc)
}