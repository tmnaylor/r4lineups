#'Confidence intervals for lineup proportion
#'
#'Computes bootstrapped confidence intervals for lineup proportion
#'@param lineup_vec A numeric vector of lineup choices
#'@param target_pos A numeric vector indexing all lineup members
#'@return Returns a vector of bias corrected confidence intervals for
#'        lineup proportion for each member in a lineup
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@details Function that computes bootstrapped lineup proportion using 1000 bootstrap draws
#'         Calls 'boot function in 'boot' package
#'@references Davison,  A.C. & Hinkley,  D.V. (1997). \emph{Bootstrap methods and their
#'            application}. Cambridge University Press.
#'
#'            Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            assessing the fairness of a lineup. \emph{Law and Human Behavior, 3}(4),
#'            285-293.
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'target_pos <- c(1, 2, 3, 4, 5, 6)
#'
#'#Call:
#'lineuprops_ci <- lineup_boot_allprop(lineup_vec, target_pos)

lineup_boot_allprop <- function(lineup_vec, target_pos){
  z <- map(target_pos,~boot(lineup_vec, lineup_prop_boot, target_pos = .x, R = 1000) %>%
             boot.ci(type = "bca")) %>%
    map(magrittr::extract, "bca") %>%
    map_df(magrittr::extract,"bca")

  z2 <-  matrix(ncol = 6,nrow = 5, z$bca) %>%
    data.frame() %>%
    slice(4:5)
  ci <- as.data.frame(t(z2))

  member <- seq(from = 1, to = length(target_pos), by = 1)
  rownames(ci) <- member
  colnames(ci) <- c("ci_low", "ci_high")
  ci <- round(ci, 3)

  return(ci)
}
