#' Read in raw buzzdetect output and optionally translate start column to real-world times.
#'
#' @param path_raw `r DOC_PARAM_PATH_RAW`
#' @param translate_to_real `r DOC_PARAM_TRANSLATE_TO_REAL`
#' @return A data.table of the raw data file; the start column will be replaced with start_real if translate_to_real is set to TRUE.
#' @export
read_raw <- function(path_raw, translate_to_real=T, tz='UTC', drop_filetime=T){
  extension <- tools::file_ext(path_raw)
  if(extension=='csv'){
    data_raw <- data.table::fread(path_raw)
  } else if(extension=='rds'){
    data_raw <- readRDS(path_raw)
  }

  names(data_raw)[names(data_raw)==COL_START_RAW] <- COL_START_FILE

  if(translate_to_real){
    file_start <- file_start_time(path_raw, tz=tz)
    realcol <- list()
    realcol[[COL_START_REAL]] <-  data_raw[[COL_START_FILE]] + file_start
    realcol <- as.data.frame(realcol)

    data_raw <- cbind(realcol, data_raw)
    if(drop_filetime){data_raw[[COL_START_FILE]] <- NULL}
  }

  data.table::setDT(data_raw)
  return(data_raw)
}


drop_activations <- function(results){
  drop_cols <- names(results)[startsWith(names(results), PREFIX_ACTIVATION)]
  if (length(drop_cols)) results[, (drop_cols) := NULL]

  return(results)
}


call_detections <- function(results, thresholds, drop=T){
  results <- data.table::copy(results)  # don't modify in place; users might want to try different thresholds
  cols_detection_out <- paste0(PREFIX_DETECTION, names(thresholds))
  cols_already_detected <- cols_detection_out[cols_detection_out %in%  names(results)]
  if(length(cols_already_detected)>0){
    plural <- ifelse(length(cols_already_detected) > 1, 's', '')
    warning(
      'Ignoring existing detection column', plural, ' ',
      paste(cols_already_detected, collapse=', ')
    )

    thresholds <- thresholds[!(paste0(PREFIX_DETECTION, names(thresholds)) %in% cols_already_detected)]
  }

  cols_to_threshold <- paste0(PREFIX_ACTIVATION, names(thresholds))
  cols_noresults <- cols_to_threshold[!(cols_to_threshold %in% names(results))]
  if(length(cols_noresults)>0){
      plural <- ifelse(length(cols_noresults) > 1, 's', '')
      warning(
        'Ignoring threshold', plural, ' given for neuron', plural, ' not found in results: ',
        paste(cols_noresults, collapse=', ')
      )

    thresholds <- thresholds[!(paste0(PREFIX_ACTIVATION, names(thresholds)) %in% cols_noresults)]
  }


  cols_to_threshold <- paste0(PREFIX_ACTIVATION, names(thresholds))
  cols_detection_out <- paste0(PREFIX_DETECTION, names(thresholds))
  if (length(thresholds) > 0) {
    results[, (cols_detection_out) := Map(`>`, mget(cols_to_threshold), as.list(thresholds))]
  }

  if(drop){
    results <- drop_activations(results)
  }

  return(results)
}


# where bincols is any known columns to bin by, probably calculated from the time columns
cols_group <- function(colnames_in, bincols){
  mask <- !(startsWith(colnames_in, PREFIX_ACTIVATION) |
      startsWith(colnames_in, PREFIX_DETECTION) |
      startsWith(colnames_in, PREFIX_DETECTIONRATE) |
      colnames_in %in% c(COL_START_REAL, COL_START_FILE, COL_BIN_REAL, COL_BIN_FILE, COL_FRAMES))

  mask[colnames_in %in% bincols] <- T

  return(colnames_in[mask])
}

cols_sum <- function(colnames_in){
  mask <- grepl("^detections_", colnames_in)
  mask[colnames_in==COL_FRAMES] <- T

  return(colnames_in[mask])
}



#' Bin a buzzdetect results file with called detections into bins by time; can re-bin previously binned results
#'
#' @param thresholds `r DOC_PARAM_THRESHOLDS`
#' @param binwidth  `r DOC_PARAM_BINWIDTH`
#' @param time_start `r DOC_PARAM_TIME_START`
#' @return A data.table with detection counts for each neuron listed in thresholds
#' @export
bin <- function(results, binwidth, calculate_rate=F){
  cnames <- names(results)
  if(COL_START_FILE  %in% cnames){
    results[[COL_BIN_FILE]] <- floor(results[[COL_START_FILE]]/(binwidth*60))*(binwidth*60)
  } else if(COL_BIN_FILE %in% cnames){
    results[[COL_BIN_FILE]] <- floor(results[[COL_BIN_FILE]]/(binwidth*60))*(binwidth*60)
  }

  if(COL_START_REAL  %in% cnames){
    results[[COL_BIN_REAL]] <- results[[COL_BIN_REAL]] <- lubridate::floor_date(results[[COL_START_REAL]], unit = paste0(binwidth, 'minutes'))
  } else if(COL_BIN_REAL %in% cnames){
    results[[COL_BIN_REAL]] <- results[[COL_BIN_REAL]] <- lubridate::floor_date(results[[COL_BIN_REAL]], unit = paste0(binwidth, 'minutes'))
  }

  if(is.null(results[[COL_BIN_FILE]]) & is.null(results[[COL_BIN_REAL]])){
    stop(
      'No compatible time column found; must have one of: ',
      paste(COL_START_FILE, COL_BIN_FILE, COL_START_REAL, COL_BIN_REAL, sep=', ')
    )
  }

  # if there aren't frames, impute them
  if(is.null(results[[COL_FRAMES]])){
    # but if there isn't a start column (only bin columns), warn the  user
    if(is.null(results[[COL_START_FILE]]) & is.null(results[[COL_START_REAL]])){
      warning('Results have neither a frames column, nor start columns. Assuming each row represents one frame.')
    }

    results[[COL_FRAMES]] <- 1
  }

  bincols <- names(results)[names(results) %in% c(COL_BIN_FILE, COL_BIN_REAL)]

  groupcols <- cols_group(names(results), bincols)
  sumcols <- cols_sum(names(results))

  # Group by all columns in group_mask and sum the specified columns
  results_bin <- results[
    ,
    c(
      lapply(sumcols, function(col) sum(.SD[[col]], na.rm = TRUE))
    ),
    by = groupcols,
    .SDcols = sumcols
  ]

  # Set proper column names for the summed columns
  data.table::setnames(results_bin,
           paste0("V", seq_along(sumcols)),
           sumcols)

  if(calculate_rate){
    for(c in names(results_bin)[startsWith(names(results_bin), PREFIX_DETECTION)]){
      c_rate <- gsub(PREFIX_DETECTION, PREFIX_DETECTIONRATE, c)
      data.table::set(results_bin, j=c_rate, value=results_bin[[c]]/results_bin[[COL_FRAMES]])
    }
  }

  return(results_bin)
}


#' Read and join all result files in a directory, optionally adding columns for parent directories.
#'
#' @param dir_in The directory holding all buzzdetect results to be analyzed.
#' @param translate_to_real `r DOC_PARAM_TRANSLATE_TO_REAL`
#' @param parent_dir_names `r DOC_PARAM_PARENT_DIR_NAMES`
#' @param results_tag `r DOC_PARAM_RESULTS_TAG`
#' @export
read_directory <- function(dir_in, translate_to_real=T, drop_filetime=T, parent_dir_names=NULL, return_filename=F, tz='UTC'){
  paths_raw <- list_matching_tag(dir_in, TAG_RESULTS)
  if(length(paths_raw)==0){
    warning(paste0('No results found in directory ', dir_in))
    return(NULL)
  }

  results <- lapply(
    X = paths_raw,
    FUN = function(path_raw){
      out <- read_raw(path_raw, translate_to_real=translate_to_real, drop_filetime=drop_filetime)
      if(!is.null(parent_dir_names)){
        elements <- path_elements(path_raw, parent_dir_names, return_filename) |>
          as.list() |>
          as.data.frame()

        out <- cbind(elements, out)
      }
      return(out)
    }
  ) |>
    data.table::rbindlist(fill = T)

  return(results)
}


#' Read all buzzdetect results in a directory, apply thresholds to call buzzes, and bin
#'
#' @inheritParams read_directory
#' @param thresholds `r DOC_PARAM_THRESHOLDS`
#' @param binwidth `r DOC_PARAM_BINWIDTH`
#' @return A data.table
#' @export
bin_directory <- function(dir_in, translate_to_real=T, drop_filetime=T, parent_dir_names=NULL, return_filename=F, tz='UTC', thresholds=c(ins_buzz=0), binwidth=5){
  results <- read_directory(
    dir_in=dir_in,
    translate_to_real=translate_to_real,
    drop_filetime=drop_filetime,
    parent_dir_names=parent_dir_names,
    return_filename=return_filename,
    tz=tz
  )

  results_called <- call_detections(results, thresholds)
  results_bin <- bin(results, binwidth=binwidth)

  return(results_bin)
}
