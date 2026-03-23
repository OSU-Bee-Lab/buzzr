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


#' Extract the identifier (ident) from a buzzdetect result file path.
#'
#' Strips the results directory prefix, the `_buzzdetect` tag, and the file
#' extension, leaving a clean relative path that uniquely identifies the
#' recording. This ident can be used to locate corresponding audio files,
#' annotation files, or metadata.
#'
#' For example, given `dir_in = '/data/results'` and
#' `path_in = '/data/results/soybean/9/230809_0000_buzzdetect.csv'`, the
#' returned ident is `'soybean/9/230809_0000'`.
#'
#' @param path_in Character vector of file paths.
#' @param dir_in The root results directory to strip from the front of each
#'   path. Defaults to `''` (returns the full path minus extension and tag).
#' @return A character vector of the same length as `path_in`.
#' @seealso [buzzr::read_directory] and [buzzr::bin_directory] which can add
#'   the ident as a column via `return_ident = TRUE`.
#' @examples
#' dir  <- system.file('extdata/five_flowers', package = 'buzzr')
#' path <- system.file(
#'   'extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv',
#'   package = 'buzzr'
#' )
#'
#' # Returns 'soybean/9/230809_0000'
#' get_ident(path, dir)
#'
#' # Works on a vector of paths
#' all_paths <- list.files(dir, pattern = '_buzzdetect', recursive = TRUE,
#'                         full.names = TRUE)
#' get_ident(all_paths, dir)
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
