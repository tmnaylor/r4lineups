#'Lineup proportion
#'
#'Computes the proportion of mock witnesses identifying a particular lineup member
#'@param lineup_table A table of lineup choices
#'@param target_pos A scalar, representing target position in lineup. Must be declared by user
#'@param k Nominal size (i.e., total number of members in lineup). Must be specified by user (scalar).
#'@return Returns a proportion indicating the frequency with which a lineup
#'        member was selected
#'@references Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'lineup_table <- table(lineup_vec)
#'
#'#Call:
#'lineup_prop_tab(lineup_table, 3, 6)
#'lineup_prop_tab(table(lineup_vec), 2, 6)
#'lineup_prop_tab(table(c(1, 2, 5, 3, 2, 4)), 2, 6)
#'
#'@export

lineup_prop_tab <- function (lineup_table,target_pos, k){
    datacheck3(lineup_table, k)
    lineup_table[target_pos]/sum(lineup_table)
}


