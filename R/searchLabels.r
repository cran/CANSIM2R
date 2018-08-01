#' searchLabels
#'
#' Helper function to search through the Hmisc labels in order to obtain the variable name mapping to their respective labels.
#' It can be particularly useful for bigger datasets for which manual exploration and visualization can be prohibitive.
#' 
#' Search is case insensitive.
#'
#' @import Hmisc
#'
#' @param df - data.frame obtained using the getCANSIM function.
#' @param pattern - the string to search for in the variable labels.
#' @examples
#' df <- getCANSIM("12-10-0005")
#' searchLabels(df, "imports")
#' @export
searchLabels <- function(df, pattern = ''){
  if(is.null(df)){
    print("Please feed a dataframe created with the getCANSIM function.")
    return()
  }
  for(i in 1:ncol(df)){
    if(grepl(pattern, label(df[[i]]), ignore.case = TRUE)){
      resultString = paste(colnames(df)[i], '-', label(df[[i]]))
      print(resultString)
    }
  }
}
