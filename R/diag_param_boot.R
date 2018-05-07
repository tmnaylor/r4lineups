#Function for getting parameters for homogeneity function from bootstrapped data
diag_param_boot <- function(lineup_pres_list, lineup_abs_list, pos_pres, pos_abs){
  diagdf1 <- as.data.frame(matrix(ncol = 2, 
                                  nrow = length(lineup_pres_list)))
  
  for (i in 1:length(lineup_pres_list)){
    diagdf1[i,1]= sum(lineup_pres_list[[i]][i] == pos_pres[i])
    diagdf1[i,2] = sum(lineup_pres_list[[i]][i] != pos_pres[i])
    
    
  }
  
  diagdf2 <- as.data.frame(matrix(ncol = 2, 
                                  nrow = length(lineup_abs_list)))
  for (i in 1:length(lineup_abs_list)){
    diagdf2[i,1]= sum(lineup_abs_list[[i]][i] == pos_abs[i])
    diagdf2[i,2] = sum(lineup_abs_list[[i]][i] != pos_abs[i]) 
    
    diagdf <- cbind(diagdf1, diagdf2)
    names(diagdf) <- c("n11", "n21", "n12", "n22")
    diagdf = as.data.frame(sapply(diagdf, as.numeric))
  }
  return(diagdf)
}
