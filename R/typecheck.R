typecheck <- function(df){
  x <- typeof(df)

  if (x == "double"){
    df = df
  }
  else{
    df <- df[[1]]}
  return(df)
}
