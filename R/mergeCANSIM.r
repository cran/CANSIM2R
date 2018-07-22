#' mergeCANSIM
#'
#' Extracts more than one CANSIM table and produces a merged table (by ID and period).
#'
#' @import reshape2 Hmisc
#'
#' @param cansimTableVector - vector containing all the CANSIM tables to extract and merge.
#' @param keepUnmatched - if true, keeps all values, even if no match was found. FALSE by default.
#' @param showLabels - show the Statistics Canada labels after finishing extracting and converting the table, TRUE by default.
#' @return data frame containing CANSIM tables.
#' @examples
#' mergeCANSIM( c(2020501, 3260021) )
#' @export
mergeCANSIM <- function(cansimTableVector, keepUnmatched = FALSE, showLabels = TRUE, lang = 'eng'){

  df <- getCANSIM(cansimTableVector[1], showLabels = FALSE, lang = lang)
  
  if(typeof(df) == 'NULL'){
    print("Please check that you can connect to the Statistics Canada website. (e.g. https://www150.statcan.gc.ca/n1/tbl/csv/23100238-eng.zip) and or that the table number is valid (please only use the first 8 digits) and try again. ")
    return(NULL)
  }

  if( length(cansimTableVector) > 1){
    for(i in 2:length(cansimTableVector) ){
      df2 <- getCANSIM(cansimTableVector[i], showLabels = FALSE)
      
      if(typeof(df2) == 'NULL'){
        print("Please check that you can connect to the Statistics Canada website. (e.g. https://www150.statcan.gc.ca/n1/tbl/csv/23100238-eng.zip) and or that the table number is valid (please only use the first 8 digits) and try again. ")
        return(NULL)
      }
      
      df2 <- renameSecondCANSIM(df2, ncol(df))

      df <- merge(df, df2, by = c("t", "i"), all = keepUnmatched )
    }
  }

  df <- df[order(df$i),]

  if(showLabels == TRUE) print( label(df) )

  return(df)
}
