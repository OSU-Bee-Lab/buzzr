#' @export
plot_quants <- function(quants, boot_conf=0.95, observation_quantiles = 0.5, time='common'){
  if(!(T %in% (c('start_common', 'start_bin') %in% names(quants)))){stop('quants has neither a start_common nor a start_bin column')}
  if(!(time %in% c('common', 'actual'))){stop("time must be either 'common' or 'actual'")}

  time <- tolower(time)
  if(time=='common'){
    col_time = 'start_common'
    if(!('start_common' %in% names(quants))){quants$start_common <- commontime(quants$start_bin)}
  } else if(time=='actual'){
    col_time = 'start_bin'
  }

  if(!('POSIXct' %in% class(quants[[col_time]]))){quants[[col_time]] <- as.POSIXct(quants[[col_time]])}

  plot_base <- ggplot2::ggplot(
    data = quants,
    ggplot2::aes(
      x = !!col_time
    )
  )

  # NOTE TO SELF: I don't need to have the color and facet cols in the function; user can do after
  # add bootstraps
  for(b in boot_conf){
    col_boot_low <- paste0('conf_', (1-boot_conf)/2)
    col_boot_hi <- paste0('conf_', 1-boot_low)
    plot_base <- plot_base +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=!!col_boot_low, ymax=!!col_boot_hi))
  }
}
