downloadCANSIM <- function(cansimTableNumber, raw = FALSE, lang){
  # validation of the lang parameter
  # thanks to Professor Jean-Herman Guay (Université de Sherbrooke) for suggesting the inclusion of French data labels
  separator = ','
  if(lang == "eng") lang = "-eng"
  else if(lang == "fra" || lang == "fr"){
    lang = "-fra"
    separator = ';'
  } 
  else {
    print("Only English (eng) and French (fra) are accepted values for lang. Defaulting to English.")
    lang = "-eng"
  }
  
  temp <- tempfile() # create a temporary file to store the downloaded data
  # create the url to download the CANSIM data according to the user's needs
  url <- "https://www150.statcan.gc.ca/n1/en/tbl/csv/"
  cansimTableNumber <- gsub('-', '', cansimTableNumber)
  cansimTableNumberString <- sprintf("%08d", as.numeric(cansimTableNumber)) #Put the correct amount of leading zeroes; paste0 uses as.character which truncates leading zeroes from integers (special thanks to Soheil soheil Mahmoodzadeh for reporting the bug)
  filename <- paste0(cansimTableNumberString, lang)
  csv_filename <- paste0(cansimTableNumberString, ".csv")
  url <- paste0(url, filename, ".zip")
  
  tryCatch(
    {
    download(url, temp, quiet = TRUE, mode = "wb") # from the downloader package, easily handles cross-plaform https requests, wrapper for download.file
    },
    error=function(err){ return(-1) },
    warning=function(warn){ return(-1) }
    )
  temp_filesize <- file.info(temp)$size
  
  if(is.na(temp_filesize) || temp_filesize == 0) return(NA) # file is non-existent, exit prematurely
  
  data <- read.csv(unz(temp, csv_filename), stringsAsFactors = FALSE, sep = separator)
  unlink(temp)
  if(raw == TRUE) return(data) #if raw equals TRUE, then the raw download is returned; functionality suggested by Soheil Mahmoodzadeh

  names(data) <- iconv(names(data), to='ASCII//TRANSLIT') # remove accents from variable names
  
  data$DGUID <- NULL
  data$IDENTIFICATEUR.D.UNITE.DE.MESURE <- NULL
  data$UOM_ID <- NULL
  data$SCALAR_FACTOR <- NULL
  data$FACTEUR.SCALAIRE <- NULL
  data$SCALAR_ID <- NULL
  data$IDENTIFICATEUR.SCALAIRE <- NULL

  data <- createStatCanVariables(data)
  
  data$VECTOR <- NULL
  data$VECTEUR <- NULL
  data$COORDINATES <- NULL
  data$COORDONEES <- NULL

  if(lang == '-fra') suppressWarnings(data$VALEUR <- as.numeric(data$VALEUR))
  else               suppressWarnings(data$VALUE <- as.numeric(data$VALUE))

  return(data)
}
