# Non-exported utils for file operations

#' List all files matching a tag (e.g., '_buzzdetect')
#' @export
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


path_elements <- function(filepath, dir_nesting, return_filename=T) {
  path_parts <- stringr::str_split(filepath, "/", simplify = T)

  end <- length(path_parts)
  start <- end - length(dir_nesting)

  elements <- path_parts[start:end]

  names(elements) <- c(dir_nesting, 'filename')

  head(elements, length(elements) - (!return_filename))
}



