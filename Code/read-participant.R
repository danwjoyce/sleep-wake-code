# read a single participant, do any necessary work on the data.frame

readParticipantCSV <- function( path, fname ) {
  d <- read.csv( file.path(path, fname) )
  # ensure Time variable is POSIX type
  d$Time <- as.POSIXct( d$Time, origin = "1970-01-01", tz = "GMT" )
  return( d )  
}

timeBin <- function( d ) {
  # takes a data frame of participant date and returns the "delta" used for binning the data
  return( as.numeric( d$Time[2] - d$Time[1] ) )
}

## example use :
# sub.fname <- "test-patient-1-svm.csv"
# d <- readParticipantCSV( data.path, sub.fname )
# delta <- timeBin( d )



