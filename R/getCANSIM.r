#' getCANSIM
#'
#' Extracts a complete CANSIM (Statistics Canada) data table
#' and converts it into a readily usable panel (wide) format.
#'
#' Geographic variables are renamed i, time variables are renamed t,
#' and all the other variables are renamed with a generic V1, V2, ..., Vn.
#' The generic variables keep the full Statistics Canada description by using a label.
#' @import reshape2 Hmisc utils
#'
#' @param cansimTableNumber - the table number we wish to retrieve from CANSIM.
#' @param showLabels - show the Statistics Canada labels after finishing extracting and converting the table, TRUE by default.
#' @param raw - download the CANSIM table as-is, skipping all processing, FALSE by default.
#' @return data frame containing CANSIM table.
#' @examples
#' getCANSIM("12-10-0005")
#' getCANSIM("12-10-0005", lang = 'fra')
#' @export
getCANSIM <- function(cansimTableNumber='', showLabels = TRUE, raw = FALSE, lang = 'eng'){
  df <- downloadCANSIM(cansimTableNumber, raw, lang)
  # temporary fix, several local and remote tests confirm that download works but CRAN server fails to connect
  # to StatCan website. This fix needs to be reviewed shortly
  if(typeof(df) == 'logical'){
    print("Please check that you can connect to the Statistics Canada website. (e.g. https://www150.statcan.gc.ca/n1/tbl/csv/23100238-eng.zip) and or that the table number is valid (please only use the first 8 digits) and try again. ")
    return(NULL)
  }
  if(raw == TRUE) return(df)

  if(lang == 'eng') df2 <- dcast(df, df[,1] + df[,2] ~ StatCanVariable, value.var = "VALUE") #function from reshape2 package
  else              df2 <- dcast(df, df[,1] + df[,2] ~ StatCanVariable, value.var = "VALEUR") 

  df2 <- df2[order(df2[,2]),]
  colnames(df2)[1] <- "t"
  colnames(df2)[2] <- "i"

  df3 <- labelCANSIM(df2)

  if(showLabels == TRUE) print( label(df3) )

  return(df3)
}
