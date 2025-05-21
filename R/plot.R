palette <- read.csv('./resources/palette.csv')
cp <- palette$hex
names(cp) <- palette$name
rm(palette)

#' @export
stepper <- function(dt, time_col, binwidth = NULL) {
  # detect binwidth if not provided
  if (is.null(binwidth)){
    times <- sort(unique(dt[[time_col]]))
    binwidth <- times[2] - times[1]
    message(paste0('No binwidth provided; binwidth automatically detected as ', binwidth, ' minutes.'))
  }

  dt[, time_col_shifted := get(time_col) + (binwidth * 0.99)]

  dt_step <- rbind(
    dt[, .SD, .SDcols = names(dt)],  # original table with all columns
    dt[, .SD, .SDcols = setdiff(names(dt), time_col)][, (time_col) := time_col_shifted]  # shifted table with updated time_col
  )

  # drop the shifted column
  dt_step[, time_col_shifted := NULL]

  return(dt_step)
}

#' @export
theme_buzzr_light <- function(base_size=10){
  ggplot2::theme_minimal(base_size) +
  ggplot2::theme(
    # Parent text settings
    #
      text = ggplot2::element_text(family="Futura-Medium", size=ggplot2::rel(5)),

    # Plot
    #
      plot.background = ggplot2::element_rect(linewidth=0),  # remove border
      plot.title = ggplot2::element_text(
        hjust = 0,
        size=ggplot2::rel(8), # Why does this need to be set separately from text?
        margin = ggplot2::margin(b=base_size/5, unit='lines')
      ),
      plot.margin = ggplot2::margin(
        t=base_size*8,
        r=base_size*9,
        b=base_size*4,
        l=base_size*5
      ),

    # Panel
    #
      panel.border = ggplot2::element_rect(linewidth=1, color = 'black', fill='transparent'),
      panel.ontop = F,
      panel.spacing.y = ggplot2::unit(base_size/4, 'lines'),


      # horizontal
      panel.grid.major.y = ggplot2::element_line(linewidth = ggplot2::rel(0.8)),
      panel.grid.minor.y = ggplot2::element_line(linewidth = ggplot2::rel(0)),

      # vertical
      panel.grid.major.x = ggplot2::element_line(linewidth = ggplot2::rel(1.5)),
      panel.grid.minor.x = ggplot2::element_line(linewidth = ggplot2::rel(0)),

    # Axes
    #
      axis.title.y = ggplot2::element_text(
        angle=0,
        hjust=1,
        vjust=0.5,
        margin = ggplot2::margin(r=ggplot2::rel(25))
      ),

    # Legend
    #
      legend.text = ggplot2::element_text(size=ggplot2::rel(4.5))#,  # Why does this need to be set separately from text?

    # Facets/strips
    #
    # strip.text = ggplot2::element_text(size=rel(1))  # Why does this need to be set separately from text? Now it seems to cause issues
  )
}

#' @export
theme_buzzr <- function(base_size=11){
  theme_buzzr_light(base_size) +
  ggplot2::theme(
    # Parent text settings
    #
    text = ggplot2::element_text(color = 'white'),

    # Plot
    #
    plot.background = ggplot2::element_rect(fill = cp[['purple_deep']]),

    # Axes
    #
      axis.text = ggplot2::element_text(color='white'),

    # Panel
    #
      panel.border = ggplot2::element_rect(linewidth=1, color = 'black'),
      panel.grid.major.x = ggplot2::element_line(color = '#4a4753'),
      panel.grid.major.y = ggplot2::element_line(color = 'white'),

    # Facets/strips
    #
    strip.text = ggplot2::element_text(color = 'white')
  )
}

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

