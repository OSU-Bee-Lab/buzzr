palette <- read.csv('./resources/palette.csv')
cp <- palette$hex
names(cp) <- palette$name
rm(palette)

#' @export
stepper <- function(dt, time_col, binwidth = NULL) {
  # detect binwidth if not provided
  if (is.null(binwidth)) {
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
theme_activitycurve <- function(){
  ggplot2::theme_minimal() +
  ggplot2::theme(
    # text
    #
    text = ggplot2::element_text(
      size=26,
      family="Futura-Medium",
      color = 'white'
    ),

    # panel
    #
    plot.background = ggplot2::element_rect( # oh...right, I put the panel on top. Will have to do in GIMP
      fill = cp[['purple_deep']],
      linewidth=0
    ),

    panel.border = element_blank(),

    # axes
    #
      # text
      axis.text = ggplot2::element_text(
        size = 24,
        color = 'white'
      ),

      axis.title.x = ggplot2::element_text(
        size = 38,
        margin =  ggplot2::margin(t=30, b = 30)
      ),

      axis.title.y = ggplot2::element_text(
        angle=0,
        hjust=1,
        vjust=0.5,
        size = 34,
        margin = ggplot2::margin(r=30)
      ),

    # grid
    panel.grid.major.x = ggplot2::element_line(
      color = '#4a4753',
      linewidth = 0.8
    ),

    panel.grid.minor.x = ggplot2::element_line(linewidth = 0),
    panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, color = 'white'),
    panel.grid.minor.y = ggplot2::element_line(linewidth = 0),
    panel.ontop = F,

    plot.margin = ggplot2::margin(2,3,0,1, 'cm'),

    strip.text = ggplot2::element_text(
      size = 24,
      color = 'white'
    ),

    plot.title = ggplot2::element_text(
      size = 40,
      margin =  ggplot2::margin(b=20),
      hjust = 0
    )
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

