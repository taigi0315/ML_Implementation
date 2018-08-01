
library(plyr)
library(arules)
#--------------------------------------------------------------------------------------------
#need library(plyr)
#need library(arules)
width_discretize <- function(dat, width){
  #copy given data to keep the original data
  o_data <- dat[,]
  n_column <- ncol(o_data)
  n_row <- nrow(o_data)
  
  #check type of data so you can know which column you would convert
  #continuos_numeric => discrete_numeric
  #factor => chracter -> integer
  
  #function which check if column has integer value or double
  check.integer <- function(N){
    !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
  }
  #check which column has factor, int value
  f_column <- which(sapply(o_data[1,], class) == "factor")
  d_column <- which( check.integer(o_data[1,]) != TRUE & sapply(o_data[1,], class) != "factor")
  #for each column with factor, save categories.
  if(length(f_column) > 0){
    for(i in 1:as.numeric(length(f_column))){#for each column that has factor data
      if(i == 1){
        category_t <- matrix(unique(o_data[,f_column[i]]), nrow = 1)
      } 
      else{
        category_t <- rbind.fill.matrix(category_t, matrix(unique(o_data[,f_column[i]]), nrow = 1))
      }
    }
  }
  #needed this part for chage factor to character for "string value column"
  o_data[f_column] <- lapply(o_data[f_column], as.character)
  #turn each factor-column into numeric value starting from "0"
  for(i in 1:nrow(category_t)){
    for(j in 1:length(unique(o_data[,f_column[i]]))){
      o_data[o_data[, f_column[i]] == category_t[i,j], f_column[i]] <-   (j-1)
    }
  }
  
  #change double value to equl frequecy-discrete
  if(length(d_column) > 0){
    for(i in 1:length(d_column)) {
      if(i == 1){
        discretize_set <- matrix(discretize(o_data[,i],  categories=width, ordered = TRUE, onlycuts = TRUE), nrow=1)
        o_data[ ,d_column[i]] <- discretize(o_data[,i],  categories=width, ordered = TRUE)
      }
      else{
        discretize_set <- rbind(discretize_set, matrix(discretize(o_data[,i],  categories=width, ordered = TRUE, onlycuts = TRUE), nrow=1))
        o_data[ ,d_column[i]] <- discretize(o_data[,i],  categories=width, ordered = TRUE)
      }
    }
    
    for(i in 1:as.numeric(length(d_column))){#make a table that saves each columns unique value
      if(i == 1){
        o_data <- o_data[order(o_data[,i]),]
        category_w <- matrix(unique(o_data[,d_column[i]]), nrow = 1)
      } 
      else{
        o_data <- o_data[order(o_data[,i]),]
        category_w <- rbind.fill.matrix(category_w, matrix(unique(o_data[,d_column[i]]), nrow = 1))
      }
    }
    
    rownames(category_w) <- d_column
    write.csv(discretize_set, file = "w_discretize_set.csv")
    #change "fact" to character
    o_data[d_column] <- lapply(o_data[d_column], as.character)
    for(i in 1:length(d_column)){
      for(j in 1:width){
        o_data[which(o_data[,i] == category_w[i,j]), i] <- (j-1)
      }
    }
  }
  return (o_data[order(o_data[,n_column]),])
}
#----------------------------------------------------------------------


