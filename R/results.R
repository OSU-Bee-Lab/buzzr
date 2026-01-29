#' Read buzzdetect results
#'
#' This function can read in results at any stage of analysis.
#' File formats can be .csv or .rds.
#' File contents can be raw buzzdetect results (.csv with a "start" column and a column for each neuron activation),
#' unbinned results (a row for each frame, start columns as start_realtime and/or start_filetime, and results columns for activations or for detections),
#' or binned results (a row for each bin, start columns as bin_realtime and/or bin_filetime, and results columns for detections).
#' The results file can be the results from one audio file or merged results (as created by [buzzr:read_directory] or [buzzr:bin_directory]).
#'
#' NOTE: "start" columns from raw buzzdetect files will be renamed to "start_filetime".
#' This enables buzzr to discriminate between frames and bins, file-times and real-world-times.
#'
#' @param path_raw `r DOC_PARAM_PATH_RESULTS`
#' @param translate_to_real `r DOC_PARAM_TRANSLATE_TO_REAL`
#' @param drop_filetime `r DOC_PARAM_DROP_FILETIME`. Ignored if translate_to_real is FALSE.
#' @param tz `r DOC_PARAM_TZ`. If the results already have a start_realtime or bin_realtime column, this will be ignored.
#' @export
read_results <- function(path_results, translate_to_real=T, drop_filetime=T, tz=NA){
  extension <- tools::file_ext(path_results)
  if(extension=='csv'){
    results <- data.table::fread(path_results)
  } else if(extension=='rds'){
    results <- readRDS(path_results)
  } else if(extension==''){
    stop('file extension not recognized for results file ', path_results, '.\n Must be .csv or .rds')
  }

  # convert buzzdetect "start" column to buzzr "start_filetime" column
  if((COL_START_RAW %in% names(results))){
    names(results)[names(results)==COL_START_RAW] <- COL_START_FILE
  }

  has_real <- (COL_START_REAL %in% names(results)) | (COL_BIN_REAL %in% names(results))
  if(translate_to_real & (!has_real)){
    file_start <- file_start_time(path_results, tz=tz)
    realcol <- list()
    realcol[[COL_START_REAL]] <-  results[[COL_START_FILE]] + file_start
    realcol <- as.data.frame(realcol)

    results <- cbind(realcol, results)
    if(drop_filetime){
      results[[COL_START_FILE]] <- NULL
      results[[COL_BIN_FILE]] <- NULL
    }
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
  setDT(results)
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
#' @param time_start `r DOC_PARAM_CALCULATE_RATE`
#' @return A data.table with a bin_ time column (bin_filetime or bin_realtime), the same detection_ columns as the input, and a frames column counting the total frames in the bin.
#' @export
bin <- function(results, binwidth, calculate_rate=F){
  binwidth_sec = binwidth*60
  cnames <- names(results)
  if(COL_START_FILE  %in% cnames){
    results[[COL_BIN_FILE]] <- floor(results[[COL_START_FILE]]/(binwidth*60))*(binwidth*60)
  } else if(COL_BIN_FILE %in% cnames){
    results[[COL_BIN_FILE]] <- floor(results[[COL_BIN_FILE]]/(binwidth*60))*(binwidth*60)
  }

  if(COL_START_REAL  %in% cnames){
    results[[COL_BIN_REAL]] <- results[[COL_BIN_REAL]] <- lubridate::floor_date(results[[COL_START_REAL]], unit = paste0(binwidth_sec, 'aseconds'))
  } else if(COL_BIN_REAL %in% cnames){
    results[[COL_BIN_REAL]] <- results[[COL_BIN_REAL]] <- lubridate::floor_date(results[[COL_BIN_REAL]], unit = paste0(binwidth_sec, 'aseconds'))
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


#' Read all buzzdetect results in a directory, recursively
#'
#' Applies [buzzr::read_results] to an entire directory, joining results.
#' optionally adding columns for parent directories.
#'
#' @param dir_results `r DOC_PARAM_DIR_RESULTS`
#' @param translate_to_real `r DOC_PARAM_TRANSLATE_TO_REAL`
#' @param parent_dir_names `r DOC_PARAM_PARENT_DIR_NAMES`
#' @param return_filename `r DOC_PARAM_RETURN_FILENAME`
#' @param return_ident `r DOC_PARAM_RETURN_IDENT`
#' @param tz `r DOC_PARAM_TZ`
#' @param results_tag `r DOC_PARAM_RESULTS_TAG`
#' @export
read_directory <- function(dir_results, translate_to_real=T, drop_filetime=T, parent_dir_names=NULL, return_ident=F, tz=NA, results_tag='_buzzdetect'){
  paths_in <- list_matching_tag(dir_results, results_tag)
  if(length(paths_in)==0){
    msg <- paste0('No results found in directory ', dir_results)

    tag_has_extension <- stringr::str_detect(results_tag, stringr::fixed('.'))
    if(tag_has_extension){
      msg <- paste0(msg, '. If your supplied results_tag contains a file extension, remove it and re-run.\nInput results tag: ', results_tag)
    }

    warning(msg)
    return(data.frame())  # return an empty data frame (doesn't mess up downstream ops like bind_rows and mutate)
  }

  results <- lapply(
    X = paths_in,
    FUN = function(path_raw){
      out <- read_results(path_raw, translate_to_real=translate_to_real, drop_filetime=drop_filetime, tz=tz)
      if(!is.null(parent_dir_names)){
        # don't return filename, redundant to returning ident
        elements <- path_elements(path_raw, parent_dir_names, return_filename=F) |>
          as.list() |>
          as.data.frame()
        out <- cbind(elements, out)
      }

      if(return_ident){
        ident <- path_raw |>
          str_remove(dir_results) |>
          str_remove("^/") |>
          tools::file_path_sans_ext(ident) |>
          str_remove(results_tag)

        out <- cbind(ident, out)
      }

      return(out)
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
bin_directory <- function(dir_in, translate_to_real=T, drop_filetime=T, parent_dir_names=NULL, return_ident=F, tz=NA, thresholds=c(ins_buzz=0), binwidth=5, calculate_rate=F, results_tag='_buzzdetect'){
  results <- read_directory(
    dir_in=dir_in,
    translate_to_real=translate_to_real,
    drop_filetime=drop_filetime,
    parent_dir_names=parent_dir_names,
    return_ident=return_ident,
    tz=tz,
    results_tag = results_tag
  )

  if(nrow(results)==0){return(data.frame())}

  results_called <- call_detections(results, thresholds)
  results_bin <- bin(results_called, binwidth=binwidth, calculate_rate = calculate_rate)

  return(results_bin)
}
