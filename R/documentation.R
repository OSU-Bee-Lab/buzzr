#' @noRd
NULL

# Parameters ----
#
  # file paths
  DOC_PARAM_PATH_RESULTS <- "The path to any buzzdetect result file that can be read with [buzzr::read_results]."
  DOC_PARAM_DIR_RESULTS <- "The directory holding all buzzdetect results to be read."
  DOC_PARAM_RESULTS_TAG <- "The tag used to identify result files during file matching, should NOT include a file extension. E.g., '_buzzdetect' instead of '_buzzdetect.csv'."
  DOC_PARAM_PARENT_DIR_NAMES <- "A vector of strings holding the column names to associate with the parent directories of the file in question. Unnamed directories will not be returned."
  DOC_PARAM_RETURN_FILENAME <- "A boolean representing whether or not to return the filename as an element/column."

  # shaping
  DOC_PARAM_RESULTS <- 'A data frame of buzzdetect results, such as created by [buzzr::read_results].'
  DOC_PARAM_THRESHOLDS <- "A named numeric vector with names corresponding to neuron names and values corresponding to the desired detection threshold for that neuron."
  DOC_PARAM_BINWIDTH <- "The desired width of the bin in minutes"
  DOC_PARAM_RETURN_IDENT <- "Whether or not to include an ident column. The ident is the relative path from your results directory to the file, not including the file extension or results tag. For example, if your results directory is models/model_general_v3/output and the result file is './foo/bar/testbuzz_buzzdetect.csv', the ident is 'foo/bar/testbuzz'."
  DOC_PARAM_CALCULATE_RATE <- 'Should the detection rate for detection_ columns be calculated? Takes total detections, divides by the number of frames, and outputs as a detectionrate_ column.'

  # misc
  DOC_PARAM_TIME_START <- "A POSIXct value (or a value that can be converted to such) identifying the date and time that the output begins. Required if the input data has no start_real column, otherwise ignored."
  DOC_PARAM_TRANSLATE_TO_REAL <- "Logical value to choose whether or not to translate start column to real-world times."

  # times
  DOC_PARAM_TIMES <- 'A vector of POSIX times.'
  DOC_PARAM_TZ <- 'The time zone for the results, as in [base::as.POSIXct].'
  DOC_PARAM_DROP_FILETIME <- 'Should the column storing the file time (start_filetime or bin_filetime) be removed from the results?'
