#'Effective Size per Foils
#'
#'Function for computing effective size (Malpass, 1981) counting foils who fall
#'within the CI for chance guessing
#'@param lineup_vec A numeric vector of lineup choices
#'@param target_pos A numeric vector indexing all lineup members
#'@param conf Desired level of alpha. Defaults to 0.95. May be specified by user (scalar).
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'target_pos <- c(1, 2, 3, 4, 5, 6)
#'
#'#Call:
#'eff_size_per_foils(lineup_vec, target_pos)
#'eff_size_per_foils(lineup_vec, target_pos, conf = 0.95)
#'@references Malpass, R S. (1981). Effective size and defendant bias in eyewitness
#'            identification lineups. \emph{Law and Human Behavior, 5}, 299-309.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup
#'            fairness. \emph{Law and Human Behavior, 22}(2), 217-237.

eff_size_per_foils <- function(lineup_vec, target_pos, conf = 0.95){
  ci <- lineup_boot_allprop(lineup_vec, target_pos, conf = 0.95)
  k <- 1/length(target_pos)
  ci_count <- cbind(ci[,1] <= k & ci[,2] >= k)
  print(sum(ci_count == TRUE))
}
