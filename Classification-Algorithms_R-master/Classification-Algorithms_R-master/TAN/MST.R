MST <- function(M.I_matrix){
  #Need package("igraph")
  flip_matrix <- 1 - M.I_matrix
  G = graph.adjacency(flip_matrix, weighted = TRUE)
  MST = minimum.spanning.tree(G)
  MST_adjacency = get.adjacency(MST, sparse = FALSE)
  return (MST_adjacency)
}

#MST_adjacecy = MST(M.I_matrix)