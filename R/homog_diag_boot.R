#'Homogeneity of diagnosticity ratio with bootstrapped CIs
#'
#'Function for computing bootstrapped estimates of homogeneity of diagnosticity ratio
#'
#'@param lineup_pres_list A list containing k vectors of lineup choices for k lineups, in which the
#'                        target was present
#'@param lineup_abs_list A list containing k vectors of lineup choices for k lineups, in which the
#'                       target was absent
#'@param pos_pres A numeric vector indexing lineup member positions for the target
#'                present condition
#'@param pos_abs A numeric vector indexing lineup member positions for the target
#'               absent condition
#'@details Computes bootstrapped diagnosticity ratio with chi-squared estimate,
#'         significance level and confidence intervals for k lineup pairs
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
#'            lineup size and lineup bias. \emph{Applied Cognitive Psychology}, 13, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.
#'@examples
#'#'@examples
#'#Target present data:
#'A <-  round(runif(100,1,6))
#'B <-  round(runif(70,1,5))
#'C <-  round(runif(20,1,4))
#'lineup_pres_list <- list(A, B, C)
#'rm(A, B, C)
#'
#'#Target absent data:
#'A <-  round(runif(100,1,6))
#'B <-  round(runif(70,1,5))
#'C <-  round(runif(20,1,4))
#'lineup_abs_list <- list(A, B, C)
#'rm(A, B, C)
#'
#'@export

homog_diag_boot <- function(lineup_pres_list, lineup_abs_list, B){
  bootdata1 <- gen_boot_samples_list(lineup_pres_list, B)
  bootdata2 <- gen_boot_samples_list(lineup_abs_list, B)
  pres_boot.dat <- lapply(bootdata1, diag_param_boot)
  abs_boot.dat <- lapply(bootdata2, diag_param_boot)
  bootlist <- mapply(cbind,pres_boot.dat, abs_boot.dat)

  listdf <- as.data.frame(bootlist)
  names<-  c("n11", "n21", "n12", "n22")
  linedf <- lapply(listdf, data.frame, stringsAsFactors = FALSE)
  linedf <- lapply(listdf, setNames, nm = names)

  par1 <- lapply(linedf, var_lnd) %>% unlist
  par2 <- lapply(linedf, ln_diag_ratio) %>% unlist
  par3 <- lapply(linedf, d_weights) %>% unlist
  par4 <- as.data.frame(cbind(par1, par2, par3))
  names(par4) <- c("var", "lnd", "wi")

  bootdata3 <- gen_boot_samples_list(par4, 100)
  par4.1 <- as.data.frame(bootdata3[1])
  par4.2 <- as.data.frame(bootdata3[2])
  par4.3 <- as.data.frame(bootdata3[3])


  chi <- lapply(par4, chi_diag)
  par6 <- pchisq(par5, df = 3, lower.tail=F)
  par7 <- d_bar(par4)

}
