checktime <- function(times, tz){
  if(!lubridate::is.POSIXct(times)){
    times <- fasttime::fastPOSIXct(times, tz)
  }

  return(times)
}
