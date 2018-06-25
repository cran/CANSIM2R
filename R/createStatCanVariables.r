createStatCanVariables <- function(df){
  VectorPosition <- match("VECTOR",names(df))

  #Only create new variable if there is more than one column from StatCan
  #Concatenates variables names accross column to create a single column (name)
  if(VectorPosition > 5) df$StatCanVariable <- apply(df[,c(3:(VectorPosition-1))], 1, function(x) paste(x, collapse = "; "))
  else df$StatCanVariable <- df[,3]

  return(df)
}
