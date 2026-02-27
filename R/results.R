#' @import data.table
NULL

convert_start_raw <- function(results){
  # convert buzzdetect "start" column to buzzr "start_filetime" column
  if((COL_START_RAW %in% names(results))){
    names(results)[names(results)==COL_START_RAW] <- COL_START_FILETIME
  }

  return(results)
}

#' Read buzzdetect results
#'
#' This function can read in results at any stage of analysis.
#' File formats can be .csv or .rds.
#' File contents can be raw buzzdetect results (.csv with a "start" column and a column for each neuron activation),
#' unbinned results (a row for each frame, start columns as start_datetime and/or start_filetime, and results columns for activations or for detections),
#' or binned results (a row for each bin, start columns as bin_datetime and/or bin_filetime, and results columns for detections).
#' The results file can be the results from one audio file or merged results (as created by [buzzr:read_directory] or [buzzr:bin_directory]).
#'
#' NOTE: "start" columns from raw buzzdetect files will be renamed to "start_filetime".
#' This enables buzzr to discriminate between frames and bins, file-times and real-world-times.
#'
#' @param path_results `r DOC_PARAM_PATH_RESULTS`
#' @param posix_formats `r DOC_PARAM_POSIX_FORMATS` If NA, leaves results in file time. See [buzzr::file_start_time].
#' @param drop_filetime `r DOC_PARAM_DROP_FILETIME` Ignored if no POSIX formats are given.
#' @param first_match `r DOC_PARAM_FIRST_MATCH`
#' @param tz `r DOC_PARAM_TZ`. If the results already have a start_datetime or bin_datetime column, this will be ignored.
#' @param dir_nesting `r DOC_PARAM_DIR_NESTING`
#' @param return_filename `r DOC_PARAM_RETURN_FILENAME`
#' @export
read_results <- function(path_results, posix_formats=NA, first_match=FALSE, drop_filetime=TRUE, tz=NA, dir_nesting=NULL, return_filename=FALSE){
  extension <- tools::file_ext(path_results)
  if(extension=='csv'){
    results <- data.table::fread(path_results)
  } else if(extension=='rds'){
    results <- readRDS(path_results)
  } else if(extension==''){
    stop('file extension not recognized for results file ', path_results, '.\n Must be .csv or .rds')
  }

  results <- convert_start_raw(results)

  has_real <- (COL_START_DATETIME %in% names(results)) | (COL_BIN_DATETIME %in% names(results))
  if((!is.na(posix_formats)) & (!has_real)){
    file_start <- file_start_time(path_results, posix_formats=posix_formats, first_match=first_match, tz=tz)
    realcol <- list()
    realcol[[COL_START_DATETIME]] <-  results[[COL_START_FILETIME]] + file_start
    realcol <- as.data.frame(realcol)

    results <- cbind(realcol, results)
    if(drop_filetime){
      results[[COL_START_FILETIME]] <- NULL
      results[[COL_BIN_FILETIME]] <- NULL
    }
  }

  elements <- c()

  if(!is.null(dir_nesting)){
    elements <- c(
      elements,
      path_elements(path_results, dir_nesting, return_filename=F)
    )
  }

  if(return_filename){
    elements <- c(
      elements,
      filename=basename(path_results)
    )
  }

  if(length(elements) > 0){
    elements <- elements |>
      as.list() |>
      as.data.frame()

    results <- cbind(elements, results)
  }

  data.table::setDT(results)
  return(results)
}

drop_activations <- function(results){
  drop_cols <- names(results)[startsWith(names(results), PREFIX_ACTIVATION)]
  if (length(drop_cols)) results[, (drop_cols) := NULL]

  return(results)
}


#' Call detections using thresholds
#'
#'
#' @param results `r DOC_PARAM_RESULTS`.
#' Must have activation_ columns corresponding to the neurons named in thresholds
#' and must *not* have detections_ column for the same.
#' @param thresholds `r DOC_PARAM_THRESHOLDS`
#' @example examples/call_detections.R
#' @export
call_detections <- function(results, thresholds){
  data.table::setDT(results)
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

  if(length(thresholds)==0){
    results <- drop_activations(results)
    return(results)
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

  if(length(thresholds)==0){
    if(drop_activations){
      results <- drop_activations(results)
    }

    return(results)
  }

  cols_to_threshold <- paste0(PREFIX_ACTIVATION, names(thresholds))
  cols_detection_out <- paste0(PREFIX_DETECTION, names(thresholds))
  if (length(thresholds) > 0) {
    results[, (cols_detection_out) := Map(`>`, mget(cols_to_threshold), as.list(thresholds))]
  }


  results <- drop_activations(results)
  return(results)
}


cols_group <- function(colnames_in){
  # group by everything
  mask <- rep(T, length(colnames_in))

  # except:
  mask[startsWith(colnames_in, PREFIX_ACTIVATION)] <- F
  mask[startsWith(colnames_in, PREFIX_DETECTION)] <- F
  mask[startsWith(colnames_in, PREFIX_DETECTIONRATE)] <- F
  mask[colnames_in %in% c(COL_START_DATETIME, COL_START_FILETIME, COL_FRAMES)] <- F

  return(colnames_in[mask])
}

cols_sum <- function(colnames_in){
  mask <- grepl("^detections_", colnames_in)
  mask[colnames_in==COL_FRAMES] <- T

  return(colnames_in[mask])
}


# floor any existing start time column; error if none present
floor_start <- function(results, binwidth){
  binwidth_sec <- binwidth*60
  results <- convert_start_raw(results)

  cnames <- names(results)

  if(COL_START_RAW %in% cnames){
    results[[COL_START_RAW]]
  }

  if(COL_START_FILETIME  %in% cnames){
    results[[COL_BIN_FILETIME]] <- floor(results[[COL_START_FILETIME]]/(binwidth_sec))*(binwidth_sec)
  } else if(COL_BIN_FILETIME %in% cnames){
    results[[COL_BIN_FILETIME]] <- floor(results[[COL_BIN_FILETIME]]/(binwidth_sec))*(binwidth_sec)
  }

  if(COL_START_DATETIME  %in% cnames){
    results[[COL_BIN_DATETIME]] <- results[[COL_BIN_DATETIME]] <- lubridate::floor_date(results[[COL_START_DATETIME]], unit = paste0(binwidth_sec, 'aseconds'))
  } else if(COL_BIN_DATETIME %in% cnames){
    results[[COL_BIN_DATETIME]] <- results[[COL_BIN_DATETIME]] <- lubridate::floor_date(results[[COL_BIN_DATETIME]], unit = paste0(binwidth_sec, 'aseconds'))
  }

  if(is.null(results[[COL_BIN_FILETIME]]) & is.null(results[[COL_BIN_DATETIME]])){
    stop(
      'No compatible time column found; must have one of: ',
      paste(COL_START_FILETIME, COL_START_DATETIME, COL_BIN_FILETIME, COL_BIN_DATETIME, sep=', ')
    )
  }

  return(results)
}


#' Bin results by time and count detections
#'
#' Given a results file, group results into bins by time and sum any detection_ columns.
#' Can re-bin previously binned results, though this gives weird results if the new bin is not a multiple of the old (e.g., bin(10), then bin(15)).
#' Optionally, calculate the detection rate for detection_ columns (simply the total detections divided by the total frames).
#' Generally, calculating detection rate is best as a final step. For example, you may first bin into 1 minute bins to compress the dataset,
#' but then re-bin by 15 minutes for graphing, re-bin by 1 hour for one model and, re-bin by 1 day for another.
#'
#' Drops all activation columns.
#'
#' @param thresholds `r DOC_PARAM_THRESHOLDS`
#' @param binwidth  `r DOC_PARAM_BINWIDTH`
#' @param calculate_rate `r DOC_PARAM_CALCULATE_RATE`
#' @return A data.table with a bin_ time column (bin_filetime or bin_datetime), the same detection_ columns as the input, and a frames column counting the total frames in the bin.
#' @export
bin <- function(results, binwidth, calculate_rate=F){
  results <- floor_start(results, binwidth)

  no_frametimes <- is.null(results[[COL_START_FILETIME]]) & is.null(results[[COL_START_DATETIME]])
  no_bintimes <- is.null(results[[COL_BIN_FILETIME]]) & is.null(results[[COL_BIN_DATETIME]])

  if(no_frametimes & no_bintimes){
    stop(
      'No time column present in results. Must have at least one of the following: ',
      paste(COL_START_FILETIME, COL_START_DATETIME, COL_BIN_FILETIME, COL_BIN_DATETIME, sep=', ')
    )
  }

  # if there aren't frames, impute them
  if(is.null(results[[COL_FRAMES]])){
    # but if the data are already binned, we must be missing the frames column for some reason. Stop.
    if(no_frametimes){  # only need to check frametimes since at this point either frametimes or bintimes must exist
      stop('Results appear to be in a binned format, but no \'frames\' column exists. Cannot bin without frame counts.')
      # I suppose we _could_ bin without frame counts, there's no mathematical reason not to. But it indicates something has gone wrong.
        # other than the mathematical reason that we may need to calculate detection rates and they could be >1 if we assume frames=1
    }

    results[[COL_FRAMES]] <- 1
  }

  groupcols <- cols_group(names(results))

  groupcols_custom <- groupcols[!(groupcols%in% c(COL_BIN_FILETIME, COL_BIN_DATETIME))]
  if(length(groupcols_custom) > 0){message('Grouping time bins using columns: ', paste(groupcols_custom, collapse= ', '))}


  sumcols <- cols_sum(names(results))  # detections and frames
  if(all(sumcols=='frames')){warning('No detection columns found in results')}

  # Group by all columns in groupcols and sum the specified columns
  results_bin <- results[
    ,
    c(
      lapply(sumcols, function(col) sum(.SD[[col]], na.rm = TRUE))
    ),
    by = groupcols,
    .SDcols = sumcols
  ]

  # Fix column names for the summed columns
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


#' Read all buzzdetect results in a directory, recursively
#'
#' Applies [buzzr::read_results] to an entire directory, joining results.
#' optionally adding columns for parent directories.
#'
#' @inheritParams read_results
#' @param dir_results `r DOC_PARAM_DIR_RESULTS`
#' @export
read_directory <- function(dir_results, posix_formats=NA, first_match=FALSE, drop_filetime=TRUE, dir_nesting=NULL, return_filename=FALSE, tz=NA){
  paths_in <- list_matching_tag(dir_results, TAG_RESULTS)
  if(length(paths_in)==0){
    msg <- paste0('No results found in directory ', dir_results)

    warning(msg)
    return(data.frame())  # return an empty data frame (doesn't mess up downstream ops like bind_rows and mutate)
  }

  results <- lapply(
    X = paths_in,
    FUN = function(path_raw){
      read_results(path_raw, posix_formats = posix_formats, first_match = first_match, drop_filetime=drop_filetime, tz=tz, dir_nesting=dir_nesting, return_filename=return_filename)
    }
  ) |>
    data.table::rbindlist(fill = T)

  return(results)
}


#' Read and bin all buzzdetect results in a directory
#'
#' Serves as a one-step implementation of [buzzr::read_directory], [buzzr::call_detections], and [buzzr::bin].
#' See documentation of these functions for details.
#'
#' @inheritParams read_directory
#' @param thresholds `r DOC_PARAM_THRESHOLDS`
#' @param binwidth `r DOC_PARAM_BINWIDTH`
#' @return A data.table
#' @export
bin_directory <- function(dir_results, thresholds, posix_formats=NA, first_match=FALSE, drop_filetime=TRUE, dir_nesting=NULL, return_filename=FALSE, tz=NA, binwidth=5, calculate_rate=FALSE){
  results <- read_directory(
    dir_results=dir_results,
    posix_formats = posix_formats,
    first_match = first_match,
    drop_filetime=drop_filetime,
    dir_nesting=dir_nesting,
    return_filename=return_filename,
    tz=tz
  )

  if(nrow(results)==0){return(data.frame())}

  results_called <- call_detections(results, thresholds)
  results_bin <- bin(results_called, binwidth=binwidth, calculate_rate = calculate_rate)

  return(results_bin)
}
