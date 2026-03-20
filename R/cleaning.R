#' @import data.table
NULL


add_activation_tag <- function(neuron){
  ifelse(grepl('^activation_', neuron), neuron, paste0('activation_', neuron))
}


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


#' @export
trim_directory <- function(dir_results, dir_trim, activation_digits, neurons_keep=NULL, if_exists='stop', workers=1){
  if_exists <- tolower(if_exists)
  if_exists <- match.arg(if_exists, c('stop', 'skip', 'overwrite'))

  paths_results <- list_results(dir_results)
  idents <- get_ident(paths_results, dir_results)

  paths_trim <- file.path(
    dir_trim,
    paste0(idents, '.rds')
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

    message('DEBUG: troubleshooting github')
    results <- read_results(path_result)
    message('DEBUG: troubleshooting github')
    foo <- c()

    results_trim <- trim_results(results, activation_digits, neurons_keep)
    saveRDS(results_trim, path_trim)
  }

  if(workers > 1){
    cl <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cl))
    parallel::clusterExport(cl, c('trim_results', 'add_activation_tag'), envir=environment())
    parallel::clusterMap(cl, trim_and_write, paths$input, paths$output)
  } else {
    mapply(trim_and_write, paths$input, paths$output)
  }

  invisible(paths$output)
}


