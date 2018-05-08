#'Diagnosticity ratio weights
#'
#'Function to compute weights of each diagnosticity ratio  for k lineup pairs
#'@param linedf A dataframe of parameters for computing diagnosticity ratio
#'@details In order to obtain a pooled estimate of a set of diagnosticity ratios,
#'         we use a weight for each ratio that is equal to the inverse of its
#'         variance.
#'
#'         \strong{To get linedf, use the diag_param helper function}
#'
#'          \emph{diag_param} returns a dataframe containing the following:
#'
#'          \itemize{
#'          \item \emph{n11}: Number of mock witnesses who identified the suspect in the target
#'               present condition
#'
#'          \item \emph{n21}: Number of mock witnesses who did not identify the suspect in the
#'              target present condition
#'
#'          \item \emph{n12}: Number of mock witnesses who identified the suspect in the target
#'              absent condition
#'
#'          \item \emph{n13}: Number of mock witnesses who did not identify the suspect in the
#'              target absent condition
#'              }
#'@return A dataframe with one column containing weights for each pair of
#'         lineups for which diagnosticity ratio is being calculated.
#'@references Tredoux, C. G. (1998). Statistical inference on measures of lineup
#'            fairness. \emph{Law and Human Behavior, 22}(2), 217-237.
#'@examples
#'#Data:
#'linedf <- diag_param(lineup_pres_list, lineup_abs_list, pos_pres, pos_abs)
#'
#'#Call:
#'wi <- d_weights(linedf)
#'
d_weights <- function(linedf){
    numerator   <- linedf$n11*linedf$n12*(linedf$n11+linedf$n21)*
                   (linedf$n12+linedf$n22)
    denominator <- linedf$n11*linedf$n22*(linedf$n11+linedf$n21)+
                   linedf$n12*linedf$n21*(linedf$n12+linedf$n22)
    wi <- numerator/denominator
    wi <- as.data.frame(wi)
    return(wi)

}
