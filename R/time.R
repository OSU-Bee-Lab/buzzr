#' Set the year, month, and date of input datetimes to 2000-01-01. Used for plotting results from different time periods on a common X axis.
#'
#' @param times Vector of POSIXct datetimes
#' @export
commontime <- function(times){
  tz <- lubridate::tz(times)
  times <- format(times, usetz=T)
  times <- stringr::str_replace(times, '^\\d{4}-\\d{2}-\\d{2}', '2000-01-01')
  times <- as.POSIXct(times, tz=tz)

  return(times)
}

time_formats <- list(
  'SonyICDPX370' = list(
    format = "%y%m%d_%H%M",
    regex = '(?<!\\d)(\\d{6}_\\d{4})(?!\\d)'
  ),

  'AudioMoth' = list(
    format = "%Y%m%d_%H%M%S",
    regex = '(?<!\\d)(\\d{8}_\\d{6})(?!\\d)'
  )
)


#' Extract timestamp from path.
#'
#' @param paths Character vector of file paths
#' @param tz What time zone should the timestamp be interpreted as? See [base::timezones]
#' @return POSIXct vector
#' @importFrom stringr str_extract
#' @export
file_start_time <- function(paths, tz) {
  extract_time <- function(path, tz){
    path_base = basename(path)
    times <- lapply(
      time_formats,
      function(f){
        t <- as.POSIXct(basename(path_base), format=f[['format']], tz=tz)
        if(is.na(t)){
          t <- str_extract(path_base, f[['regex']], group=1) |>
            as.POSIXct(format=f[['format']], tz=tz)
        }

        return(t)
      }
    )

    n_match <- sum(!is.na(times))

    if(n_match > 1){
      warning('multiple time formats compatible with file ', path, '\n returning NA')
      return(NA)
    }

    if(n_match==0){
      warning('no time formats compatible with file ', path, '\n returning NA')
      return(NA)
    }

    times[!is.na(times)][[1]] |> # drop names (USE.NAMES = F doesn't do this already for some reason)
      as.POSIXct()
  }

  times = sapply(paths, extract_time, tz=tz, USE.NAMES = F) |>
    as.POSIXct()

  return(times)
}

#' Extract timestamp from path. Assumes YYYYMMDD_HHMMSS format, as used by AudioMoth recorders.
#'
#' @param path_raw Character vector of file paths
#' @param tz Timezone string
#' @return POSIXct vector
#' @importFrom stringr str_extract
file_start_time_AudioMoth <- function(path_raw, tz=NA) {
  timestamps <- str_extract(basename(path_raw), "\\d{8}_\\d{6}")
  as.POSIXct(timestamps, format = patterns[['AudioMoth']], tz = tz)
}
