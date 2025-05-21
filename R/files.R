list_matching_tag <- function(dir_in, tag) {
  # Get all file paths
  paths_all <- list.files(
    dir_in,
    recursive = TRUE,
    full.names = TRUE
  )

  # Remove extensions
  paths_dropext <- tools::file_path_sans_ext(paths_all)

  paths_matching <- paths_all[endsWith(paths_dropext, tag)]

  return(paths_matching)
}


#' Given a path, extract the start time as POSIXct object. Assumes YYMMDD_HHMM format, as used by Sony ICD-PX370 recorders.
#'
#' @param path_raw The file path of any file with a name starting with a YYMMDD_HHMM timestamp
#' @return A POSIXct object bearing the start time of the file
#' @export
file_start_time <- function(path_raw){
  filename <- basename(path_raw)
  pattern <- paste0("^\\d{6}_\\d{4}")
  matches <- regexec(pattern, filename)
  timestamp <- regmatches(filename, matches)[[1]]
  start_real <- as.POSIXct(timestamp, format = "%y%m%d_%H%M", tz='America/New_York')

  return(start_real)
}


recdir_to_elements <- function(dir_recorder, intermediate_dirs){
  path_split <- strsplit(dir_recorder, split='/')[[1]]

  element_start <- length(path_split) - length(intermediate_dirs)
  elements <- path_split[element_start:length(path_split)]
  names(elements) <- c(intermediate_dirs, 'recorder')

  return(elements)
}


