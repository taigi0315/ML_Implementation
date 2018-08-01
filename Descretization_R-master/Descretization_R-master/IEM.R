Ent <- function(A, S)  {#given data and attribute(column)
  S_w = S
  #order whole data by A (increasing)
  #calculate prop for each class
  n_col <- ncol(S_w)
  n_class <- length(unique(S_w[,n_col]))
  #gets prop for each class level
  PT <- prop.table(table(S[,n_col])) #table for class prob
  Ent_result = 0 #set 0 for summation, calculate Ent formula
  for(i in 1:n_class){
    if(PT[i] == 0){PT[i] = 0.001}
    Ent_result <- Ent_result + PT[i]*log2(PT[i])
  }
  return(-Ent_result)
}
#==========================================================================================
E_ATS <- function(A,T,S){
  #calculate E(A,T,S)
  S_w = S
  S_1 = S_w[S_w[,A] <= T, ]
  S_2 = S_w[S_w[,A] > T, ]
  if(nrow(S_1) == 0){ Ent_S_1 = 0 }
  else{ Ent_S_1 = nrow(S_1)/nrow(S_w)*Ent(A,S_1) }
  if(nrow(S_2) == 0){ Ent_S_2 = 0 }
  else{ Ent_S_2 = nrow(S_2)/nrow(S_w)*Ent(A,S_2) }
  
  E_ATS_result = Ent_S_1 + Ent_S_2
  return(E_ATS_result)
}
#==========================================================================================
T_finder <- function(A,S){
  S_w = S
  T_table = matrix(100, ncol = 3, nrow = (nrow(S_w)-1) )   
  T_table[,1] = S_w[1:(nrow(S_w)-1),A]
  T_table[,2] = diff(S_w[,A])/2
  
  for(i in 1:nrow(T_table)){
    if(T_table[i,2] > 0){
      T_value = T_table[i,1] + T_table[i,2]
      T_table[i,3] = E_ATS(A,T_value,S_w)
    }
  }
  Min_index = which(T_table[,3] == min(T_table[,3]) )
  Min_T = (T_table[Min_index,1] + T_table[Min_index,2])
  return (Min_T)
}
#==========================================================================================
Gain_checker <- function(A,T,S){
  S_w = S[,]
  S_1 = S_w[S_w[,A] <= T, ]
  S_2 = S_w[S_w[,A] > T, ]
  
  k_w <- length(unique(S[,ncol(S)]))
  k_1 <- length(unique(S_1[,ncol(S_1)]))
  k_2 <- length(unique(S_2[,ncol(S_2)]))
  
  gain = Ent(A,S_w) - E_ATS(A,T,S_w)
  delta = log2((3^k_w) - 2) - ( k_w*Ent(A,S_w) - k_1*Ent(A,S_1) - k_2*Ent(A,S_2) )
  N = nrow(S_w)
  log_value = (log2(N-1) / N) + (delta/N)
  
  if(gain > log_value){  #recursion needed
    return (TRUE)
  }
  else{                 #recursion is not needed
    return(FALSE)
  }
}
#==========================================================================================
IEM_recursion <- function(A,Ts,S){
  S_w = S
  minimum_T = T_finder(A,S_w)
  gain <- Gain_checker(A,minimum_T,S_w)
  if(gain == TRUE){
    #new minimum_T is included in Ts
    Ts = c(Ts,minimum_T)
    Ts = unique(Ts)
    upper_T = minimum_T
    bottom_T = minimum_T
    
    S_1 = S_w[S_w[,A] <= minimum_T, ]
    S_2 = S_w[S_w[,A] > minimum_T, ]
    if(length(unique(S_1[,ncol(S_1)])) > 1 & length(unique(S_1[,A])) > 1){ #if #class = 1 or #element = 1 , no recursion
      upper_T = IEM_recursion(A,Ts,S_1)
    }
    if(length(unique(S_2[,ncol(S_2)])) > 1 &length(unique(S_2[,A])) > 1){
      bottom_T = IEM_recursion(A,Ts,S_2)
    }
    return (c(Ts,upper_T,bottom_T))
  }
  else{
    return (Ts)
  }
}
#==========================================================================================
IEM <- function(S){
  library(plyr)
  dis_S = S[,]
  #dis_S[,6] = as.numeric(as.character(dis_S[,6]))
  S_o <- dis_S #save for comparison
  
  #function which check if column has integer value or double
  check.integer <- function(N){
    !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = TRUE))
  }
  #check which column has factor, int value
  f_column <- which(sapply(S_o[1,], class) == "factor")
  d_column <- which( check.integer(S_o[1,]) != TRUE & sapply(S_o[1,], class) != "factor")
  #manually fix -_-;;
  #f_column <- c()
  #d_column <- c(2:8)
  
  #for each column with factor, save categories.
  if(length(f_column) > 0){
    for(i in 1:as.numeric(length(f_column))){#for each column that has factor data
      if(i == 1){
        category_t <- matrix(unique(S_o[,f_column[i]]), nrow = 1)
      } 
      else{
        category_t <- rbind.fill.matrix(category_t, matrix(unique(S_o[,f_column[i]]), nrow = 1))
      }
    }
    #needed this part for chage factor to character for "string value column"
    S_o[f_column] <- lapply(S_o[f_column], as.character)
    
    #turn each factor-column into numeric value starting from "0"
    for(i in 1:nrow(category_t)){
      for(j in 1:length(unique(S_o[,f_column[i]]))){
        S_o[S_o[, f_column[i]] == category_t[i,j], f_column[i]] <-   (j-1)
      }
    }
    S_o[f_column] <- lapply(S_o[f_column], as.numeric)
  }
  dis_S[,f_column] = S_o[,f_column]
  
  #column that need to be discretized
  if(length(d_column) > 0){
    for(i in 1:length(d_column)){
      S_o <- S_o[order(S_o[,d_column[i]]), ]
      #recursively find best cut points
      min_value = min(S_o[ ,as.numeric(d_column[i])])
      max_value = max(S_o[ ,as.numeric(d_column[i])])
      cutpoint_table <- IEM_recursion(d_column[i],min_value,S_o)
      cutpoint_table <- cutpoint_table[!duplicated(cutpoint_table)]
      cutpoint_table = c(cutpoint_table,max_value)
      cutpoint_table <- sort(cutpoint_table)
      for(cut_index in 1:(length(cutpoint_table)-1)){
        dis_S[(cutpoint_table[cut_index] <= dis_S[ ,d_column[i]] & dis_S[ ,d_column[i]] <= cutpoint_table[(cut_index+1)]), d_column[i] ] <- ((cut_index-1)+10000000)
      }
      dis_S[,d_column[i]] = (dis_S[,d_column[i]] - 10000000)
      print(paste(d_column[i],cutpoint_table,sep = "::") )
    }
    
  }
  return (dis_S)
}
#==========================================================================================