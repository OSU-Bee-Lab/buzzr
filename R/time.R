#' @export
commontime <- function(times, tz){
  times <- format(times, usetz=T)
  times <- stringr::str_replace(times, '^\\d{4}-\\d{2}-\\d{2}', '2000-01-01')
  times <- fasttime::fastPOSIXct(times, tz)
}
