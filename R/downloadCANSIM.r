downloadCANSIM <- function(cansimTableNumber, raw = FALSE){
  temp <- tempfile()
  url <- "https://www150.statcan.gc.ca/n1/en/tbl/csv/"
  cansimTableNumber <- gsub('-', '', cansimTableNumber)
  cansimTableNumberString <- sprintf("%08d", as.numeric(cansimTableNumber)) #Put the correct amount of leading zeroes; paste0 uses as.character which truncates leading zeroes from integers (special thanks to Soheil soheil Mahmoodzadeh for reporting the bug)
  filename <- paste0(cansimTableNumberString, "-eng")
  csv_filename <- paste0(cansimTableNumberString, ".csv")
  url <- paste0(url, filename, ".zip")
  
  # temporary fix, several local and remote tests confirm that download works but CRAN server fails to connect
  # to StatCan website. This fix needs to be reviewed shortly
  tryCatch(
    {
    download(url, temp, quiet = TRUE, mode = "wb") # from the downloader package, easily handles cross-plaform https requests, wrapper for download.file
    },
    error=function(err) { return(-1) },
    warning=function(warn){ return(-1) }
    )
  temp_filesize <- file.info(temp)$size
  
  if(is.na(temp_filesize) || temp_filesize == 0) return(NA) # file is non-existent, exit prematurely
  
  data <- read.csv(unz(temp, csv_filename), stringsAsFactors = FALSE)
  unlink(temp)
  if(raw == TRUE) return(data) #if raw equals TRUE, then the raw download is returned; functionality suggested by Soheil Mahmoodzadeh

  data$DGUID <- NULL
  data$UOM_ID <- NULL
  data$SCALAR_FACTOR <- NULL
  data$SCALAR_ID <- NULL

  data <- createStatCanVariables(data)

  data$VECTOR <- NULL
  data$COORDINATE <- NULL
  suppressWarnings(data$VALUE <- as.numeric(data$VALUE))

  return(data)
}
