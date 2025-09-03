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


#' @importFrom stringr str_split
recdir_to_elements <- function(dir_recorder, intermediate_dirs) {
  path_parts <- stringr::str_split(dir_recorder, "/", simplify = TRUE)
  n <- length(path_parts)
  start <- n - length(intermediate_dirs)

  elements <- path_parts[start:n]
  names(elements) <- c(intermediate_dirs, "recorder")

  elements
}



