#' Read in raw buzzdetect output (this is currently just a wrapper for data.table::fread)
#'
#' @param path_raw The file path of the buzzdetect output
#' @return A data.table
#' @export
read_raw <- function(path_raw){
  dt_raw <- data.table::fread(
    path_raw
  )

  return(dt_raw)
}


#' Bin raw buzzdetect output by time
#'
#' @param dt_raw A data.table holding raw detection values, in the _buzzdetect.csv format
#' @param time_start A POSIXct value (or a value that can be converted to such) identifying the date and time that the output begins
#' @param thresholds A named vector where the names correspond to buzzdetect classes and their values correspond to the desired detection threshold for that class.
#' @param binwidth The desired with of the bin in minutes, to be passed to the "unit" argument of lubridate::floor_date()
#' @return A data.table
#' @export
bin_raw <- function(dt_raw, time_start, thresholds=c('ins_buzz'=-1), binwidth=5){
  time_start <- as.POSIXct(time_start)

  dt_raw$start_real <- time_start + dt_raw$start
  dt_raw$start_bin <- lubridate::floor_date(dt_raw$start_real, unit = paste0(binwidth, 'minutes'))

  cols_out <- paste0("detections_", cols_bin)

  dt_bin <- dt_raw[
    ,
    c(
      .N,
      lapply(names(thresholds), function(col) sum(.SD[[col]] > thresholds[col]))
    ),
    by = start_bin,
    .SDcols = names(thresholds)
  ]

  names(dt_bin) <- c('start_bin', 'frames', names(thresholds))

  return(dt_bin)
}


#' Re-bin existing binned detections
#'
#' @param dt_bin A data.table in the binned buzzdetect format
#' @param binwidth The desired with of the bin in minutes, to be passed to the "unit" argument of lubridate::floor_date()
#' @return A data.table
#' @export
rebin <- function(dt_bin, binwidth){
  # TODO: catch when input binwidth is less than existing binwidth
  dt_rebin <- dt_bin
  dt_rebin$start_bin <- lubridate::floor_date(dt_bin$start_bin, unit = paste0(binwidth, 'minutes'))

  dt_rebin <- dt_rebin[
    ,
    lapply(.SD, sum),
    by = start_bin,
    .SDcols = !c('start_bin')
  ]

  return(dt_rebin)
}

