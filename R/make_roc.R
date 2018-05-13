#'Compute and plot ROC curve for lineup accuracy ~ confidence
#'
#'Function to compute and plot an ROC curve for data from an eyewitness
#'experiment, where accuracy is recorded for target present and target
#'absent lineups
#'
#'This function takes a df with two columns, confidence and acc
#'where acc = binary accuracy.
#'It returns an ROC object of package pROC
#'
#'The approach is outlined in several papers by Mickes, Wixted, Gronlund,
#'Clark, and others (see references)
#'
#'@param adf A dataframe with two columns, named confidence and acc
#'@examples
#'make_roc(mickwick)
#'
#'
#'
#'@references Gronlund, S. D., Wixted, J. T., & Mickes, L. (2014). Evaluating
#'eyewitness identification procedures using receiver operating characteristic
#'analysis. \emph{Current Directions in Psychological Science, 23}(1), 3-10.
#'
#'@details This function is a user level function.  It chains the two roc functions
#'together.  The user must pass a dataframe, with one column indicating
#'confidence, and another accuracy, and these must be named

make_roc <- function(adf){
  make_rocdata(adf) %>%
    make_roc_gg() -> rocplot
  return(rocplot)
}
