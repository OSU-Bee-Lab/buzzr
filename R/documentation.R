#' @noRd
NULL

# Parameters ----
#
  # file paths
  DOC_PARAM_PATH_RESULTS <- "The path to any buzzdetect result file that can be read with [buzzr::read_results]."
  DOC_PARAM_DIR_RESULTS <- "The directory holding all buzzdetect results to be read."
  DOC_PARAM_DIR_NESTING <- "A character vector used to name the directory levels above the results file. Each element becomes a column in the output, storing the components of the path. For example, dir_nesting = c('site', 'recorder') for the path data/2026/siteA/recorder_4/250704_11343_buzzdetect.csv would add a site column (holding the value 'siteA') and a recorder column (holding the value 'recorder_4')."
  DOC_PARAM_RETURN_FILENAME <- "A boolean representing whether or not to return the filename as an element/column."

  # shaping
  DOC_PARAM_RESULTS <- 'A data frame of buzzdetect results, such as created by [buzzr::read_results].'
  DOC_PARAM_THRESHOLDS <- "A named numeric vector with names corresponding to neuron names and values corresponding to the desired detection threshold for that neuron."
  DOC_PARAM_BINWIDTH <- "The desired width of the bin in minutes"
  DOC_PARAM_CALCULATE_RATE <- 'Should the detection rate for detection_ columns be calculated? Takes total detections, divides by the number of frames, and outputs as a detectionrate_ column.'

  # misc
  DOC_PARAM_TIME_START <- "A POSIXct value (or a value that can be converted to such) identifying the date and time that the output begins. Required if the input data has no start_real column, otherwise ignored."

  # times
  DOC_PARAM_TIMES <- 'A vector of POSIX times.'
  DOC_PARAM_TZ <- 'The time zone for the results, as in [base::as.POSIXct].'
  DOC_PARAM_DROP_FILETIME <- 'Should the column storing the file time (start_filetime or bin_filetime) be removed from the results?'
  DOC_PARAM_FIRST_MATCH <- 'If multiple formats match, should the time be returned as NA (FALSE) or should the first matching format be accepted (TRUE)?'
  DOC_PARAM_POSIX_FORMATS <- 'Character vector of POSIX format codes to convert the file name to a start time.'
