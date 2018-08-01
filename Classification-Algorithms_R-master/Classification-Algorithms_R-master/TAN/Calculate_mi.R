Calculate_mi <- function(dat){
  n_feat <- (ncol(dat)-1)
  n_row <- nrow(dat)
  M.I_matrix <- matrix(0, nrow=n_feat, ncol=n_feat)
  for(i in 1:(n_feat-1)){  #i,j are index for X_i, X_j
    for(j in (i+1):n_feat){ 
      number.of.x = length(unique(dat[,i])) #get unique value in X_i, X_j 
      number.of.y = length(unique(dat[,j]))
      M.I = 0
      table_i = prop.table( table(dat[,i]) )
      table_j = prop.table( table(dat[,j]) )
      table_ij = prop.table( table(dat[,i], dat[,j]) )
      table_ij[which(table_ij == 0 )] = 1/n_row
      for(x in 1:number.of.x){
        for(y in 1:number.of.y){
          M.I = M.I + ( table_ij[x,y] * (log2(table_ij[x,y] / (table_i[x] * table_j[y]) )) )
        }
      }
      M.I_matrix[i,j] = M.I
    }
  }
  return (M.I_matrix)
}

#M.I_matrix = Calculate_mi(dat)
