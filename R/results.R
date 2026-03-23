#' @import data.table
NULL

convert_start_raw <- function(results){
  # convert buzzdetect "start" column to buzzr "start_filetime" column
  if((COL_START_RAW %in% names(results))){
    names(results)[names(results)==COL_START_RAW] <- COL_START_FILETIME
  }

  return(results)
}

#' Read a single buzzdetect results file.
#'
#' Reads a buzzdetect result file at any stage of analysis using
#' [data.table::fread] (`.csv`) or [base::readRDS] (`.rds`).
#' The raw buzzdetect `"start"` column is renamed to `"start_filetime"` so that
#' buzzr can tell apart file-relative timestamps (seconds from the start of the
#' audio file) from real-world date-times.
#'
#' Optionally, the recording's start date-time is parsed from the file name and
#' used to convert `start_filetime` into an absolute `start_datetime` column.
#' Directory levels above the file can also be extracted into their own columns
#' via `dir_nesting`, which is particularly useful when combining many files
#' with [buzzr::read_directory].
#'
#' @param path_results `r DOC_PARAM_PATH_RESULTS`
#' @param posix_formats `r DOC_PARAM_POSIX_FORMATS`
#'   If `NA` (default), results are left in file-time. See [buzzr::file_start_time].
#' @param drop_filetime `r DOC_PARAM_DROP_FILETIME` Ignored if no POSIX formats are given.
#' @param first_match `r DOC_PARAM_FIRST_MATCH`
#' @param tz `r DOC_PARAM_TZ`. Ignored if the results already contain a
#'   `start_datetime` or `bin_datetime` column.
#' @param dir_nesting `r DOC_PARAM_DIR_NESTING`
#' @return A data.table with a `start_filetime` or `start_datetime` column,
#'   one column per neuron activation or detection, and optionally one column
#'   per level of `dir_nesting`.
#' @seealso [buzzr::read_directory] to read all files in a folder at once,
#'   [buzzr::call_detections] to apply detection thresholds,
#'   [buzzr::bin] to summarise results into time bins.
#' @examples
#' path <- system.file(
#'   'extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv',
#'   package = 'buzzr'
#' )
#'
#' # Basic read — 'start' column is renamed to 'start_filetime'
#' read_results(path)
#'
#' # Parse real-world date-time from the filename (YYMMDD_HHMM format)
#' read_results(path, posix_formats = '%y%m%d_%H%M', tz = 'America/New_York')
#'
#' # Keep both time columns and label directory levels
#' read_results(
#'   path,
#'   posix_formats  = '%y%m%d_%H%M',
#'   tz             = 'America/New_York',
#'   drop_filetime  = FALSE,
#'   dir_nesting    = c('flower', 'recorder')
#' )
#' @export
read_results <- function(path_results, posix_formats=NA, first_match=FALSE, drop_filetime=TRUE, tz=NA, dir_nesting=NULL){
  if(!file.exists(path_results)){
    stop('File does not exist: ', path_results)
  }

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
      dir_levels(path_results, dir_nesting)
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


#' Call event detections using activation thresholds.
#'
#' Converts raw neuron activation values into binary detections by applying a
#' numeric threshold to each named neuron. Frames where the activation exceeds
#' the threshold are marked `TRUE`; all others are `FALSE`. All `activation_`
#' columns are dropped from the output — use [buzzr::bin] afterwards to
#' summarise detection counts over time.
#'
#' The original data frame is never modified; a copy is returned.
#'
#' @param results `r DOC_PARAM_RESULTS`. Must contain `activation_` columns for
#'   each neuron named in `thresholds`.
#' @param thresholds `r DOC_PARAM_THRESHOLDS`
#'   Activations *above* the threshold value are counted as detections, so
#'   thresholds for models that output negative log-likelihoods (such as
#'   `model_general_v3`) are typically negative (e.g. `-1.2`).
#' @return A data.table with `detections_` columns replacing the `activation_`
#'   columns. Each detection column contains logical (`TRUE`/`FALSE`) values.
#' @seealso [buzzr::bin] to count detections per time bin,
#'   [buzzr::bin_directory] to run the full pipeline in one call.
#' @examples
#' path <- system.file(
#'   'extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv',
#'   package = 'buzzr'
#' )
#' results <- read_results(path)
#'
#' # Single neuron: activations above -1.2 are counted as buzz detections
#' call_detections(results, thresholds = c(ins_buzz = -1.2))
#'
#' # Multiple neurons at once
#' call_detections(results, thresholds = c(ins_buzz = -1.2, mech_plane = -2.0))
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


#' Bin results by time, summing detections and frames per bin.
#'
#' Groups frame-level results into fixed-width time bins and sums all
#' `detections_` columns and the `frames` column within each bin. Any remaining
#' columns (e.g. from `dir_nesting`) are used as grouping variables, so bins
#' are computed separately for each unique combination of those columns.
#'
#' Can also re-bin previously binned results. For cleanest results, choose a
#' new `binwidth` that is a multiple of the original (e.g. re-bin 5-minute bins
#' into 60-minute bins). Non-multiple re-binning is allowed but produces
#' boundary artefacts.
#'
#' Detection rates (`detectionrate_` columns) are best calculated at the final
#' binning step. If you plan to re-bin, leave `calculate_rate = FALSE` until
#' then.
#'
#' @param results `r DOC_PARAM_RESULTS`. Must contain at least one time column
#'   (`start_filetime`, `start_datetime`, `bin_filetime`, or `bin_datetime`)
#'   and at least one `detections_` column.
#' @param binwidth `r DOC_PARAM_BINWIDTH`
#' @param calculate_rate `r DOC_PARAM_CALCULATE_RATE`
#' @return A data.table with a `bin_filetime` or `bin_datetime` column, summed
#'   `detections_` columns, a `frames` column, and optionally `detectionrate_`
#'   columns. All other input columns are retained as grouping variables.
#' @seealso [buzzr::call_detections] to produce the `detections_` columns that
#'   this function expects, [buzzr::frames_expected] to check whether your bins
#'   contain the right number of frames.
#' @examples
#' path <- system.file(
#'   'extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv',
#'   package = 'buzzr'
#' )
#' results <- read_results(path, posix_formats = '%y%m%d_%H%M', tz = 'America/New_York')
#' called  <- call_detections(results, thresholds = c(ins_buzz = -1.2))
#'
#' # 15-minute bins
#' bin(called, binwidth = 15)
#'
#' # With detection rates
#' binned <- bin(called, binwidth = 15, calculate_rate = TRUE)
#'
#' # Re-bin to 1-hour windows for a coarser view
#' bin(binned, binwidth = 60)
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

  groupcols_custom <- groupcols[!(groupcols %in% c(COL_BIN_FILETIME, COL_BIN_DATETIME, COL_IDENT))]
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


#' Read all buzzdetect result files in a directory (recursively).
#'
#' Applies [buzzr::read_results] to every buzzdetect result file found
#' recursively under `dir_results` and row-binds the results into a single
#' data.table. All arguments are forwarded to [buzzr::read_results].
#'
#' Returns an empty `data.frame` (with a warning) if no result files are found,
#' which allows safe use in pipelines with [data.table::rbindlist] or similar.
#'
#' @inheritParams read_results
#' @param dir_results `r DOC_PARAM_DIR_RESULTS`
#' @param return_ident `r DOC_PARAM_RETURN_IDENT` Set `TRUE` to add an `ident`
#'   column as the first column of the output.
#' @return A data.table combining all files, with the same columns as
#'   [buzzr::read_results] plus an optional `ident` column.
#' @seealso [buzzr::bin_directory] to also apply thresholds and bin in one call.
#' @examples
#' dir <- system.file('extdata/five_flowers', package = 'buzzr')
#'
#' # Read all five files and combine into one data.table
#' read_directory(
#'   dir,
#'   posix_formats = '%y%m%d_%H%M',
#'   tz            = 'America/New_York',
#'   dir_nesting   = c('flower', 'recorder')
#' )
#'
#' # Also include the ident column for tracing results back to their source file
#' read_directory(dir, return_ident = TRUE)
#' @export
read_directory <- function(dir_results, posix_formats=NA, first_match=FALSE, drop_filetime=TRUE, dir_nesting=NULL, return_ident=FALSE, tz=NA){
  paths_in <- list_results(dir_results)
  if(length(paths_in)==0){
    msg <- paste0('No results found in directory ', dir_results)

    warning(msg)
    return(data.frame())  # return an empty data frame (doesn't mess up downstream ops like bind_rows and mutate)
  }

  results <- lapply(
    X = paths_in,
    FUN = function(path_results){
      df <- read_results(path_results, posix_formats = posix_formats, first_match = first_match, drop_filetime=drop_filetime, tz=tz, dir_nesting=dir_nesting)

      if(return_ident){
        ident <- get_ident(path_results, dir_results)

        # add as first column
        df <- cbind(ident=ident, df)
      }

      return(df)
    }
  ) |>
    data.table::rbindlist(fill = T)

  return(results)
}


#' Read, threshold, and bin all buzzdetect result files in a directory.
#'
#' A convenience wrapper that runs [buzzr::read_directory],
#' [buzzr::call_detections], and [buzzr::bin] in sequence. All arguments from
#' those functions are accepted here. See their documentation for details.
#'
#' This is the recommended entry point for most analyses: point it at your
#' results folder, supply your thresholds and time zone, and get back a tidy
#' binned data.table ready for plotting or modelling.
#'
#' @inheritParams read_directory
#' @param thresholds `r DOC_PARAM_THRESHOLDS`
#' @param binwidth `r DOC_PARAM_BINWIDTH`
#' @param calculate_rate `r DOC_PARAM_CALCULATE_RATE`
#' @return A data.table with `bin_filetime` or `bin_datetime`, `detections_`
#'   columns, a `frames` column, and any columns from `dir_nesting` or `ident`.
#' @seealso [buzzr::read_directory], [buzzr::call_detections], and [buzzr::bin]
#'   for the individual steps this function wraps,
#'   [buzzr::frames_expected] to check bins for missing audio coverage.
#' @examples
#' dir <- system.file('extdata/five_flowers', package = 'buzzr')
#'
#' bin_directory(
#'   dir_results    = dir,
#'   thresholds     = c(ins_buzz = -1.2),
#'   posix_formats  = '%y%m%d_%H%M',
#'   tz             = 'America/New_York',
#'   dir_nesting    = c('flower', 'recorder'),
#'   binwidth       = 20,
#'   calculate_rate = TRUE
#' )
#' @export
bin_directory <- function(dir_results, thresholds, posix_formats=NA, first_match=FALSE, drop_filetime=TRUE, dir_nesting=NULL, return_ident=FALSE, tz=NA, binwidth=5, calculate_rate=FALSE){
  results <- read_directory(
    dir_results=dir_results,
    posix_formats = posix_formats,
    first_match = first_match,
    drop_filetime=drop_filetime,
    dir_nesting=dir_nesting,
    return_ident=return_ident,
    tz=tz
  )

  if(nrow(results)==0){return(data.frame())}

  results_called <- call_detections(results, thresholds)
  results_bin <- bin(results_called, binwidth=binwidth, calculate_rate = calculate_rate)

  return(results_bin)
}
