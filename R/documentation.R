#' @noRd
NULL

# Parameters ----
#
  # file paths
  DOC_PARAM_PATH_RESULTS <- "The path to any buzzdetect result file that can be read with [buzzr::read_results]."
  DOC_PARAM_DIR_RESULTS <- "The directory holding all buzzdetect results to be read."
  DOC_PARAM_DIR_NESTING <- "A character vector used to name the directory levels above the results file. Each element becomes a column in the output, storing the components of the path. For example, dir_nesting = c('site', 'recorder') for the path data/2026/siteA/recorder_4/250704_11343_buzzdetect.csv would add a site column (holding the value 'siteA') and a recorder column (holding the value 'recorder_4')."
  DOC_PARAM_RETURN_IDENT <- "The 'ident' is the relative path from your data directory to the results file, without the _buzzdetect tag or file extension. Useful for finding corresponding audio files or annotations."

  # shaping
  DOC_PARAM_RESULTS <- 'A data frame or data.table of buzzdetect results, such as created by [buzzr::read_results].'
  DOC_PARAM_THRESHOLDS <- "A named numeric vector mapping neuron names to detection thresholds (e.g. `c(ins_buzz = -1.2)`). Frames whose activation value *exceeds* the threshold are counted as detections. Because `model_general_v3` outputs negative log-likelihoods, thresholds are typically negative."
  DOC_PARAM_BINWIDTH <- "Width of each time bin in minutes (e.g. `5`, `20`, or `60`)."
  DOC_PARAM_CALCULATE_RATE <- "If `TRUE`, adds a `detectionrate_` column for each `detections_` column, calculated as detections divided by frames. Values range from 0 to 1."

  # misc
  DOC_PARAM_TIME_START <- "A POSIXct value (or a value that can be converted to such) identifying the date and time that the output begins. Required if the input data has no start_real column, otherwise ignored."

  # times
  DOC_PARAM_TIMES <- 'A POSIXct vector.'
  DOC_PARAM_TZ <- "Time zone string passed to [base::as.POSIXct] (e.g. `'America/New_York'`). See `OlsonNames()` for valid values."
  DOC_PARAM_DROP_FILETIME <- "If `TRUE` (default), the `start_filetime` / `bin_filetime` column is removed once `start_datetime` / `bin_datetime` has been added. Set `FALSE` to keep both."
  DOC_PARAM_FIRST_MATCH <- "Controls behaviour when multiple formats produce *different* times for the same file. `FALSE` (default) returns `NA` with a warning; `TRUE` accepts the time from the first matching format with a message."
  DOC_PARAM_POSIX_FORMATS <- "Character vector of POSIX format strings (see [base::strptime]) describing the timestamp embedded in each file name (e.g. `'%y%m%d_%H%M'` for `230809_0600`). Supply multiple strings when recordings from different logger types are mixed in one directory."
