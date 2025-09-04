#' @noRd
NULL

# Parameters ----
#
  # file paths
  DOC_PARAM_PATH_RAW <- "The file path to the raw buzzdetect output file. The filetype can be csv or rds."
  DOC_PARAM_DIR_RECORDER <- "The file path to the recorder directory"
  DOC_PARAM_RESULTS_TAG <- "The tag used to identify result files during file matching, should NOT include a file extension. E.g., _buzzdetect instead of _buzzdetect.csv"
  DOC_PARAM_PARENT_DIR_NAMES <- "A vector of strings holding the column names to associate with the parent directories of the file in question. Unnamed directories will not be returned."
  DOC_PARAM_RETURN_FILENAME <- "A boolean representing whether or not to return the filename as an element/column."

  # data
  DOC_PARAM_DATA_RAW <- "A data.frame holding raw detection values, in the _buzzdetect.csv format"
  DOC_PARAM_DATA_BIN <- "A data.frame in the binned buzzdetect format"

  # analysis
  DOC_PARAM_THRESHOLDS <- "A named numeric vector with names corresponding to neuron names and values corresponding to the desired detection threshold for that neuron"
  DOC_PARAM_BINWIDTH <- "The desired width of the bin in minutes"

  # misc
  DOC_PARAM_TIME_START <- "A POSIXct value (or a value that can be converted to such) identifying the date and time that the output begins. Required if the input data has no start_real column, otherwise ignored"
  DOC_PARAM_TRANSLATE_TO_REAL <- "Logical value to choose whether or not to translate start column to real-world times"


# Returns ----
#
  DOC_RETURN_DATA_TABLE <- "A data.frame"
