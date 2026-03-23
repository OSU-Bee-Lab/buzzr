#' A simplified scale_x_datetime label that returns only the hour of the day (for use with [buzzr::commontime]).
#'
#' @param tz Time zone string to use when formatting hours (e.g. `'America/New_York'`).
#' Should match the `tz` argument passed to [buzzr::commontime]. Defaults to your system timezone.
#' @examples
#' # ggplot2::scale_x_datetime(labels=buzzr::label_hour(tz='America/New_York'))
#' @returns A *function* that takes POSIX values and returns only their hours. See examples for use.
#' @export
label_hour <- function(tz = Sys.timezone()){
  label_fn <- function(breaks){
    strftime(breaks, format = "%I %p", tz=tz) |>
      stringr::str_remove('^0') |>
      tolower()
  }

  return(label_fn)
}


#' The buzzdetect color palette.
#'
#' This is named, discrete version of a magma color palette (magma is frequently used for spectrograms).
#' See [viridis::magma] for a continuous version.
#'
#' @export
palette <- {
  palette_df <- read.csv('./resources/palette.csv')
  p <- palette_df$hex
  names(p) <- palette_df$name
  p
}


#' A ggplot theme for aesthetic plotting of buzzdetect results.
#'
#' @param base_size What should the size of the text elements on the plot be?
#' @param mode Should the theme take on a light or a dark color palette?
#' @examples
#' # (some ggplot code) +
#' # theme_buzzr(base_size=14, mode='light')
#'
#' @returns A ggplot2 theme
#' @export
theme_buzzr <- function(base_size=10, mode='light'){
  if(mode=='light'){
    return(theme_buzzr_light(base_size))
  }

  if (mode == 'dark'){
    return(theme_buzzr_dark(base_size))
  }

  stop('Theme mode must be light or dark')
}

theme_buzzr_light <- function(base_size){
  ggplot2::theme_minimal(base_size) +
    ggplot2::theme(

      text = ggplot2::element_text(family="Futura-Medium", size=base_size),

      # Plot
      plot.background = ggplot2::element_rect(linewidth=0),  # remove border from plots (panels still have them)
      plot.title = ggplot2::element_text(hjust = 0.5),

      # Panel
      panel.border = ggplot2::element_rect(linewidth=1, color = 'black', fill='transparent'),
      panel.ontop = F,

      # Gridlines
      panel.grid.major.y = ggplot2::element_line(linewidth = ggplot2::rel(0.8)),
      panel.grid.minor.y = ggplot2::element_line(linewidth = ggplot2::rel(0)),

      panel.grid.major.x = ggplot2::element_line(linewidth = ggplot2::rel(1.5)),
      panel.grid.minor.x = ggplot2::element_line(linewidth = ggplot2::rel(0)),

      # Axes
      axis.title.y = ggplot2::element_text(
        angle=0,  # read without tilting your head
        hjust=1,  # right align
        vjust=0.5, # vertical center
        margin=ggplot2::margin(r=base_size)
      ),

      axis.title.x = ggplot2::element_text(hjust=0.5),

      # Strips (facet_grid, facet_wrap)
      panel.spacing.y = grid::unit(12, 'pt'),
      strip.text.x = ggplot2::element_text(angle=0),
      strip.text.y = ggplot2::element_text(angle=0, hjust=0)
    )
}


theme_buzzr_dark <- function(base_size){
  theme_buzzr_light(base_size) +
  ggplot2::theme(
    # Parent text settings
    #
    text = ggplot2::element_text(color = 'white'),

    # Plot
    #
    plot.background = ggplot2::element_rect(fill = palette[['purple_deep']]),

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
