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
trim_dir <- function(dir_results, dir_trim, activation_digits, neurons_keep=NULL, overwrite=FALSE, workers=1){
  paths_results <- list_results(dir_results)

  # build output paths by replacing input root with output root
  paths_trim <- file.path(dir_trim, substring(paths_results, nchar(dir_results) + 2))

  # check for existing files before doing any work
  existing <- paths_trim[file.exists(paths_trim)]
  if(length(existing) > 0){
    if(!overwrite){
      stop('Output files already exist. Set overwrite=TRUE to overwrite:\n', paste(existing, collapse='\n'))
    } else {
      warning('Overwriting ', length(existing), ' existing file(s).')
    }
  }

  trim_and_write <- function(path_result, path_trim){
    # create output directory if needed
    dir.create(dirname(path_trim), recursive=TRUE, showWarnings=FALSE)

    results <- readRDS(path_result)
    results_trim <- trim_results(results, activation_digits, neurons_keep)
    saveRDS(results_trim, path_trim)
  }

  if(workers > 1){
    cl <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cl))
    parallel::clusterExport(cl, c('trim_results', 'add_activation_tag'), envir=environment())
    parallel::clusterMap(cl, trim_and_write, paths_results, paths_trim)
  } else {
    mapply(trim_and_write, paths_results, paths_trim)
  }

  invisible(paths_trim)
}


