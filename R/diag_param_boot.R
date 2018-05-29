diag_param_boot <- function(lineup_pres_list){
  pos_pres <- list(NULL)
  for (i in 1:length(lineup_pres_list)){
    pos_pres[[i]]= c(sort(unique(lineup_pres_list[[i]])))
  }

  diagdf <- as.data.frame(matrix(ncol = 2,
                                  nrow = length(lineup_pres_list)))



  for (i in 1:length(lineup_pres_list)){
    diagdf[i,1]= sum(lineup_pres_list[[i]] == pos_pres[[i]])
    diagdf[i,2] = sum(lineup_pres_list[[i]] != pos_pres[[i]])
     diagdf= as.data.frame(sapply(diagdf, as.numeric))
  }
  return(diagdf)

}
