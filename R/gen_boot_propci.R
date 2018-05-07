#'Percentile of Bootstrapped Lineup Proportion
#'
#'Function for computing arbitrary percentile of bootstrapped lineup proportion
#'
#'@param lineuprops A vector of bootstrapped lineup proportions
#'@param perc Percentile to be computed. Must be declared by user (scalar)
#'
#'            Defaults to .05
#'@return Arbitrary percentile of bootstrapped lineup proportion
#'@details Can be used to calculate confidence intervals at desired level of alpha
#'@examples
#'Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'target_pos <- 3
#'
#'Bootstrap data:
#'bootdf <- gen_boot_samples(lineup_vec, 1000)
#'
#'Compute proportion for bootstrapp samples:
#'lineuprops <- gen_lineup_prop(lineup_boot_df, target_pos)
#'
#'Get boot CIs:
#'prop_bootci_lower <- gen_boot_propci(lineuprops, perc = .025)
#'prop_bootci_upper <- gen_boot_propci(lineuprops, perc = .975)

gen_boot_propci <- function (lineuprops, perc=.05){
    if(perc >= 0 & perc <= 1) {
        quantile(lineuprops, probs = perc)
    }
    else {
        cat("Illegal value entered (must be proportion)")
    }
}
