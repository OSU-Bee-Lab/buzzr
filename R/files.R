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
path_elements <- function(filepath, parent_dir_names, return_filename=T) {
  path_parts <- stringr::str_split(filepath, "/", simplify = T)

  end <- length(path_parts)
  start <- end - length(parent_dir_names)

  elements <- path_parts[start:end]

  names(elements) <- c(parent_dir_names, 'filename')

  head(elements, length(elements) - (!return_filename))
}



