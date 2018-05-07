#'Chi-squared estimate of homogeneity of diagnosticity ratio
#'
#'Function for getting chi-squared value for homogeneity of diagnosticity ratios
#'@param df A dataframe containing: ln(d), variance of ln(d), d weights
#'@details To compute linedf, use the diag_param helper function.
#'         To compute df, apply ln_diag_ratio, var_lnd & d_weights functions
#'         to linedf, then bind results into one dataframe
#'@examples
#'#Parameters for diagnosticity calculations
#'linedf <- diag_param(lineup_pres_list, lineup_abs_list, pos_pres, abs_pres)
#'
#'#Get ln(d), variance of ln(d) & d weights
#'ratio <- ln_diag_ratio(linedf)
#'var <- var_lnd(linedf)
#'wi <- d_weights(linedf)
#'
#'#Bind estimates into one dataframe
#'df <- cbind(ratio, var, wi)
#'
#'#Call:
#'chi_diag(df)
#'
#'@references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'
#'            Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
#'            construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
#'            & M. P. Toglia (Eds.), \emph{Handbook of Eyewitness Psychology, Vol. 2: Memory for
#'            people} (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'            \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures of
#'            lineup size and lineup bias. \emph{Applied Cognitive Psychology, 13}, S9-S26.
#'
#'            Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.

chi_diag <- function(df){
    q <- sum(((df$lnd-log(d_bar(df)))^2)/(df$var))
    return(q)
}
