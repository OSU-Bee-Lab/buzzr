# Non-exported utils for file operations
list_results <- function(dir_in) {
  # Get all file paths
  paths_all <- list.files(
    dir_in,
    recursive = TRUE,
    full.names = TRUE
  )

  # Remove extensions
  paths_dropext <- tools::file_path_sans_ext(paths_all)

  paths_matching <- paths_all[endsWith(paths_dropext, TAG_RESULTS)]

  return(paths_matching)
}


dir_levels <- function(filepath, dir_nesting) {
  path_parts <- stringr::str_split(filepath, "/", simplify = T)

  end <- length(path_parts)
  start <- end - length(dir_nesting)

  elements <- path_parts[start:end]

  names(elements) <- c(dir_nesting, 'filename')

  head(elements, length(elements) - 1)
}


#' @export
get_ident <- function(path_in, dir_in=''){
  path_in |>
    # no extension
    tools::file_path_sans_ext() |>

    # no results tag
    stringr::str_remove(paste0(TAG_RESULTS, '$')) |>

    # remove the data dir
    stringr::str_remove(paste0('^', stringr::fixed(dir_in))) |>

    # remove any leading slash
    stringr::str_remove('^/')
}
