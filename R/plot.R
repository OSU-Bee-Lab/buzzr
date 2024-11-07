palette <- read.csv('./resources/palette.csv')

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


stepper_bak <- function(dt, binwidth=NULL){
  if(is.null(binwidth)){
    times <- sort(unique(dt$start_bin))
    binwidth <- times[2] - times[1]
    message(paste0('No binwidth provided; binwidth automatically detected as ', binwidth, ' minutes.'))
  }

  dt[, start_bin_shifted := start_bin + (binwidth * 0.99)]

  dt_step <- rbind(
    dt[, .SD, .SDcols = names(dt)],  # original table with all columns
    dt[, .SD, .SDcols = setdiff(names(dt), 'start_bin')][, start_bin := start_bin_shifted]  # shifted table with updated start_bin
  )

  # drop shifted column
  dt_step[, start_bin_shifted := NULL]

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
      fill = palette$hex[palette$name=='purple_deep']
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
      color = "darkgray",
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
