#' @export
commontime <- function(times){
  times <- format(times, usetz=T)
  times <- stringr::str_replace(times, '^\\d{4}-\\d{2}-\\d{2}', '2000-01-01')
  times <- as.POSIXct(times)

  return(times)
}
