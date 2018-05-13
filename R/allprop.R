#'Lineup proportion for all lineup members
#'
#'Computes lineup proportion for each member in a lineup
#'@param lineup_vec A numeric vector of lineup choices
#'@param target_pos A numeric vector indexing all lineup members
#'@param conf Desired level of alpha. Defaults to 0.95. May be specified by user (scalar).
#'@return Returns a vector containing lineup proportion for each lineup member
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'target_pos <- c(1, 2, 3, 4, 5, 6)
#'
#'#Call:
#'x <- allprop(lineup_vec, target_pos)
#'@references Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'             assessing the fairness of a lineup. \emph{Law and Human Behavior, 3}(4),
#'             285-293.

allprop <- function(lineup_vec, target_pos){
    propvec <- as.data.frame(matrix(ncol= 1,
                                    nrow = length(target_pos)))
    for (i in 1:length(target_pos)){
        propvec[i,]=lineup_prop_vec(lineup_vec, target_pos[i])
    }
    return(propvec)
}
