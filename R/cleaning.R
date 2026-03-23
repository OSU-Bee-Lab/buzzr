#' @import data.table
NULL


add_activation_tag <- function(neuron){
  ifelse(grepl('^activation_', neuron), neuron, paste0('activation_', neuron))
}


#' Trim activation columns in a results data frame.
#'
#' Rounds all `activation_` columns to the specified number of decimal places.
#' Optionally retains only the neurons named in `neurons_keep`, dropping all others.
#' This is useful for reducing file size before archiving or sharing results.
#' The original data frame is not modified.
#'
#' @param results A data frame or data.table with `activation_` columns.
#' @param activation_digits Integer. Number of decimal places to round activation values to.
#' @param neurons_keep Character vector of neuron names to retain. Names may include or omit
#'   the `activation_` prefix (e.g. `"ins_buzz"` and `"activation_ins_buzz"` are equivalent).
#'   All other activation columns are dropped. If `NULL` (default), all neurons are kept.
#' @return A data.table with rounded (and optionally filtered) activation columns.
#' @seealso [buzzr::trim_directory] to apply this to an entire folder of files.
#' @examples
#' path <- system.file(
#'   'extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv',
#'   package = 'buzzr'
#' )
#' results <- read_results(path)
#'
#' # Round all activation columns to 2 decimal places
#' trim_results(results, activation_digits = 2)
#'
#' # Keep only the ins_buzz neuron and round to 1 decimal place
#' trim_results(results, activation_digits = 1, neurons_keep = 'ins_buzz')
#' @export
trim_results <- function(results, activation_digits, neurons_keep=NULL){
  results_trim <- data.table::copy(results)
  if(!is.null(neurons_keep)){
    # initialize mask with every activation column
    drop_mask <- stringr::str_detect(names(results_trim), '^activation_')
    # don't drop activation columns matching any neurons_keep
    keep_cols <- add_activation_tag(neurons_keep)

    missing_neurons <- keep_cols[!(keep_cols %in% names(results_trim))]
    if(length(missing_neurons) > 0){
      warning('neurons_keep contains neurons not found in results: ', paste(missing_neurons, collapse = ', '))
    }

    drop_mask <- drop_mask & !(names(results_trim) %in% keep_cols)
    if(inherits(results_trim, "data.table")){
      results_trim <- results_trim[, !drop_mask, with = FALSE]
    } else {
      results_trim <- results_trim[, !drop_mask]
    }
  }
  # round all remaining activation columns
  cols_round <- grep("^activation_", names(results_trim))
  if(inherits(results_trim, "data.table")){
    results_trim[, (cols_round) := lapply(.SD, round, digits = activation_digits), .SDcols = cols_round]
  } else {
    results_trim[, cols_round] <- lapply(results_trim[, cols_round], round, digits = activation_digits)
  }
  return(results_trim)
}


#' Trim activation columns for all result files in a directory.
#'
#' Applies [buzzr::trim_results] to every buzzdetect result file found recursively
#' in `dir_results`, saving each trimmed file in `dir_trim` while preserving the
#' original directory structure.
#'
#' @param dir_results Path to the directory containing buzzdetect result files.
#' @param dir_trim Path to the output directory. Created automatically if it does not exist.
#' @param activation_digits Integer. Number of decimal places to round activation values to.
#' @param neurons_keep Character vector of neuron names to retain (see [buzzr::trim_results]).
#'   If `NULL` (default), all neurons are kept.
#' @param output_format Output file format. One of `"rds"` (default) or `"csv"`.
#'   CSV files are written without row names.
#' @param if_exists What to do if an output file already exists.
#'   One of `"stop"` (default, throws an error), `"skip"` (silently skips existing files),
#'   or `"overwrite"` (overwrites with a warning).
#' @param workers Number of parallel workers. Defaults to `1` (sequential).
#'   Parallelism uses [parallel::mcmapply] and may not be supported on all platforms.
#' @return Invisibly returns a character vector of output file paths.
#' @seealso [buzzr::trim_results] for the single-file version.
#' @examples
#' \dontrun{
#' dir_in  <- system.file('extdata/five_flowers', package = 'buzzr')
#' dir_out <- file.path(tempdir(), 'five_flowers_trimmed')
#'
#' # Trim all files, rounding to 2 decimal places, saved as .rds
#' trim_directory(dir_in, dir_out, activation_digits = 2)
#'
#' # Save trimmed files as CSV instead
#' trim_directory(dir_in, dir_out, activation_digits = 2, output_format = 'csv')
#'
#' # Re-run, keeping only the ins_buzz neuron and overwriting existing files
#' trim_directory(
#'   dir_in, dir_out,
#'   activation_digits = 2,
#'   neurons_keep = 'ins_buzz',
#'   if_exists = 'overwrite'
#' )
#' }
#' @export
trim_directory <- function(dir_results, dir_trim, activation_digits, neurons_keep=NULL, output_format='rds', if_exists='stop', workers=1){
  if_exists <- tolower(if_exists)
  if_exists <- match.arg(if_exists, c('stop', 'skip', 'overwrite'))
  output_format <- tolower(output_format)
  output_format <- match.arg(output_format, c('rds', 'csv'))

  paths_results <- list_results(dir_results)
  idents <- get_ident(paths_results, dir_results)

  paths_trim <- file.path(
    dir_trim,
    paste0(idents, '.', output_format)
  )

  paths <- data.frame(input=paths_results, output=paths_trim)

  # handle existing output files
  paths$exists <- file.exists(paths$output)
  if(any(paths$exists)){
    if(if_exists == 'skip'){
      message('Skipping ', sum(paths$exists), ' file(s) that already exist.')
      paths <- paths[!paths$exists, ]
    } else if(if_exists == 'stop'){
      stop('Output files already exist. Set if_exists to "overwrite" or "skip":\n',
           paste(paths$output[paths$exists], collapse='\n'))
    } else if(if_exists == 'overwrite'){
      warning('Overwriting ', sum(paths$exists), ' existing file(s).')
    }
  }

  paths$exists <- NULL

  if(nrow(paths) == 0){
    message('No files to process.')
    return(invisible(character(0)))
  }

  trim_and_write <- function(path_result, path_trim){
    # create output directory if needed
    dir.create(dirname(path_trim), recursive=TRUE, showWarnings=FALSE)
    results <- read_results(path_result)

    results_trim <- trim_results(results, activation_digits, neurons_keep)
    if(output_format == 'csv'){
      data.table::fwrite(results_trim, path_trim)
    } else {
      saveRDS(results_trim, path_trim)
    }
  }

  if(workers > 1){
    parallel::mcmapply(trim_and_write, paths$input, paths$output,
                       SIMPLIFY = FALSE, mc.cores = workers)
  } else {
    mapply(trim_and_write, paths$input, paths$output)
  }

  invisible(paths$output)
}


