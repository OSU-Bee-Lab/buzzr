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
#' @param thresholds A named vector where the names correspond to buzzdetect classes and their values correspond to the desired detection threshold for that class.
#' @param binwidth The desired with of the bin in minutes, to be passed to the "unit" argument of lubridate::floor_date()
#' @param time_start A POSIXct value (or a value that can be converted to such) identifying the date and time that the output begins. Required if the input data has no start_real column, otherwise ignored.
#' @return A data.table
#' @export
bin_raw <- function(dt_raw, thresholds=c('ins_buzz'=-1), binwidth=5, time_start=NA){
  dt_raw <- data.table::as.data.table(dt_raw)

  # TODO: give warning when time_start and $start_real both exist
  if(is.null(dt_raw$start_real)){
    if(is.na(time_start)){stop('input data has no start_real column, so you must provide a value for time_start')}
    time_start <- as.POSIXct(time_start)
    dt_raw$start_real <- time_start + dt_raw$start
  }

  dt_raw$start_bin <- lubridate::floor_date(dt_raw$start_real, unit = paste0(binwidth, 'minutes'))

  dt_bin <- dt_raw[
    ,
    c(
      .N,
      lapply(names(thresholds), function(col) sum(.SD[[col]] > thresholds[col]))
    ),
    by = start_bin,
    .SDcols = names(thresholds)
  ]

  names(dt_bin) <- c('start_bin', 'frames', paste0('detections_', names(thresholds)))

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
  if(is.null(dt_bin$start_bin)){
    stop('input data has no start_bin value')
  }

  if(length(grep("^detections_", names(dt_bin)))==0){
    stop('input data has no detections_ columns')
  }

  dt_bin <- data.table::as.data.table(dt_bin)
  if(!lubridate::is.POSIXct(dt_bin$start_bin)){
    dt_bin$start_bin <- fasttime::fastPOSIXct(dt_bin$start_bin)
  }

  dt_rebin <- dt_bin
  dt_rebin$start_bin <- lubridate::floor_date(dt_bin$start_bin, unit = paste0(binwidth, 'minutes'))

  value_cols <- c('frames', grep("^detections_", names(dt_rebin), value = TRUE))

  dt_rebin <- dt_rebin[
    ,
    lapply(.SD, sum),

    # group by non-values
    by = setdiff(names(dt_rebin), value_cols),

    # summarize values
    .SDcols = value_cols
  ]

  return(dt_rebin)
}

