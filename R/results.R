#' Read in raw buzzdetect output and optionally translate start column to real-world times.
#'
#' @param path_raw `r DOC_PARAM_PATH_RAW`
#' @param translate_to_real `r DOC_PARAM_TRANSLATE_TO_REAL`
#' @return A data.table of the raw data file; the start column will be replaced with start_real if translate_to_real is set to TRUE.
#' @export
read_raw <- function(path_raw, translate_to_real=T){
  extension <- tools::file_ext(path_raw)
  if(extension=='csv'){
    data_raw <- data.table::fread(path_raw)
  } else if(extension=='rds'){
    data_raw <- readRDS(path_raw)
  }

  if(translate_to_real){
    file_start <- file_start_time(path_raw)
    data_raw$start <- data_raw$start + file_start
    names(data_raw)[names(data_raw)=='start'] <- 'start_real'
  }

  return(data_raw)
}


#' Bin a raw buzzdetect output into bins by time, calling detections according to the input thresholds.
#'
#' @param data_raw `r DOC_PARAM_DATA_RAW`
#' @param thresholds `r DOC_PARAM_THRESHOLDS`
#' @param binwidth  `r DOC_PARAM_BINWIDTH`
#' @param time_start `r DOC_PARAM_TIME_START`
#' @return A data.table with detection counts for each neuron listed in thresholds
#' @export
bin_raw <- function(data_raw, thresholds=c(ins_buzz=0), binwidth=5, time_start=NA){
  # TODO: add ability to group within non-neuron, non-start times, as does rebin()
  data_raw <- data.table::as.data.table(data_raw)

  # TODO: give warning when time_start and $start_real both exist
  if(is.null(data_raw$start_real)){
    if(is.na(time_start)){stop('input data has no start_real column, so you must provide a value for time_start')}
    time_start <- as.POSIXct(time_start)
    data_raw$start_real <- time_start + data_raw$start
  }

  data_raw$start_bin <- lubridate::floor_date(data_raw$start_real, unit = paste0(binwidth, 'minutes'))

  data.bin <- data_raw[
    ,
    c(
      .N,
      lapply(names(thresholds), function(col) sum(.SD[[col]] > thresholds[col]))
    ),
    by = start_bin,
    .SDcols = names(thresholds)
  ]

  names(data.bin) <- c('start_bin', 'frames', paste0('detections_', names(thresholds)))

  return(data.bin)
}


#' Re-bin already-binned detections.
#'
#' @param data_bin `r DOC_PARAM_DATA_BIN`
#' @param binwidth `r DOC_PARAM_BINWIDTH`
#' @export
rebin <- function(data_bin, binwidth){
  # TODO: catch when input binwidth is less than existing binwidth
  has_detectionrate <- 'detectionrate' %in% names(data_bin)
  if(has_detectionrate){data_bin$detectionrate <- NULL}

  has_time_common <- 'time_common' %in% names(data_bin)
  if(has_time_common){data_bin$time_common <- NULL}


  if(is.null(data_bin$start_bin)){
    stop('input data has no start_bin value')
  }

  if(length(grep("^detections_", names(data_bin)))==0){
    stop('input data has no detections_ columns')
  }

  cnames <- names(data_bin)
  name_mask <- rep(T, ncol(data_bin))
  name_mask[cnames %in% c('start_bin', 'frames')] <- F
  name_mask[grepl('detections_', cnames)] <- F

  if(any(name_mask)){message('using the following columns as grouping variables: ', paste(cnames[name_mask], collapse = ', '))}

  data_bin <- data.table::as.data.table(data_bin)

  if(!lubridate::is.POSIXct(data_bin$start_bin)){
    data_bin$start_bin <- as.POSIXct(data_bin$start_bin)
  }

  data_rebin <- data_bin
  data_rebin$start_bin <- lubridate::floor_date(data_bin$start_bin, unit = paste0(binwidth*60, ' aseconds'))

  value_cols <- c('frames', grep("^detections_", names(data_rebin), value = TRUE))

  data_rebin <- data_rebin[
    ,
    lapply(.SD, sum),

    # group by non-values
    by = setdiff(names(data_rebin), value_cols),

    # summarize values
    .SDcols = value_cols
  ]

  if(has_detectionrate){data_rebin$detectionrate <- data_rebin$detections_ins_buzz/data_rebin$frames}
  if(has_time_common){data_rebin$time_common <- commontime(data_rebin$start_bin)}
  return(data_rebin)
}

#' Read all files in a recorder directory and return the binned results
#'
#' @param dir_recorder `r DOC_PARAM_DIR_RECORDER`
#' @param intermediate_dirs `r DOC_PARAM_INTERMEDIATE_DIRS` description
#' @param thresholds `r DOC_PARAM_THRESHOLDS`
#' @param binwidth `r DOC_PARAM_BINWIDTH`
#' @param results_tag `r DOC_PARAM_RESULTS_TAG`
#' @return A data.table
#' @export
bin_recorder <- function(dir_recorder, intermediate_dirs = NULL, thresholds=c(ins_buzz=0), binwidth=5, results_tag = "_buzzdetect"){
  results <- lapply(
    X = list_matching_tag(dir_recorder, results_tag),
    FUN = read_raw,
    translate_to_real = T
  )

  results <- data.table::rbindlist(results)
  results_bin <- bin_raw(results, thresholds=thresholds, binwidth=binwidth)

  elements <- recdir_to_elements(dir_recorder, intermediate_dirs)
  results_bin <- cbind(
    as.data.frame(as.list(elements)),
    results_bin
  )

  return(results_bin)
}


#' Read and join all result files in a directory, appending the names of directories between the dir_experiment and the recorder_dir.
#'
#' @param dir_experiment The directory holding all buzzdetect results to be analyzed.
#' @inheritParams bin_recorder
#' @export
bin_experiment <- function(dir_experiment, intermediate_dirs, thresholds=c(ins_buzz=0), binwidth=5, results_tag="_buzzdetect"){
  paths_results <- list_matching_tag(dir_experiment, results_tag)
  dirs_recorders <- unique(dirname(paths_results))

  results <- lapply(
    X = dirs_recorders,
    FUN = bin_recorder,
    intermediate_dirs = intermediate_dirs,
    thresholds = thresholds,
    binwidth = binwidth,
    results_tag = results_tag
  )

  results <- data.table::rbindlist(results)

  return(results)
}

