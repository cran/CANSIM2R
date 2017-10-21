downloadCANSIM <- function(cansimTableNumber, raw = FALSE){
  temp <- tempfile()
  url <- "http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/"
  cansimTableNumberString <- sprintf("%07d", as.numeric(cansimTableNumber)) #Put the correct amount of leading zeroes; paste0 uses as.character which truncates leading zeroes from integers (special thanks to Soheil soheil Mahmoodzadeh for reporting the bug)
  filename <- paste0("0", cansimTableNumberString, "-eng")
  url <- paste0(url, filename, ".zip")
  download.file(url, temp, quiet = TRUE)
  data <- read.csv(unz(temp, paste0(filename, ".csv") ), stringsAsFactors = FALSE)
  unlink(temp)
  if(raw == TRUE) return(data) #if raw equals TRUE, then the raw download is returned; functionality suggested by Soheil Mahmoodzadeh
  data <- createStatCanVariables(data)
  
  data$Vector <- NULL
  data$Coordinate <- NULL
  suppressWarnings(data$Value <- as.numeric(data$Value))
  
  return(data)
}