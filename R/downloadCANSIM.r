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
  
  file_content <- readChar(temp, temp_filesize)
  isZip <- !grepl('<html', file_content, fixed = TRUE)

  if(!isZip){ #more processing needed, a token may need to be retrieved to get the actual download link for the zip file containing the csv
    # StatsCan recently changed how their system works. Now their download link for the zip file leads to another website with
    # a new link containing a ?st=random_token appended to the url, which forces us to have to retrieve it dynamically in order
    # to fetch the zip file containing the csv file. After checking if the downloaded "zip file" is actually HTML, we use regex
    # to obtain the token from the HTML file served and then use it to create the new working URL and finally download the real zip
    # file.
    # example url: https://www150.statcan.gc.ca/n1/en/tbl/csv/18100256-eng.zip
    # final working url: https://www150.statcan.gc.ca/n1/en/tbl/csv/18100256-eng.zip?st=MGT-bXlM
    downloadToken <- regmatches(file_content, regexpr("(?<=\\.zip\\?st=)(.*)(?=\"\\s)", file_content, perl = TRUE))
    newURL <- paste0(url, "?st=", downloadToken)
    download(newURL, temp, quiet = TRUE, mode = "wb")
  }

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
