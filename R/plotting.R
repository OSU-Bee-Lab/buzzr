#' ggplot2 x-axis label formatter for time-of-day plots.
#'
#' Returns a labeller function that formats POSIXct values as readable hour
#' strings (e.g. `"6 am"`, `"2 pm"`). Designed for use with
#' [ggplot2::scale_x_datetime] after converting your time column with
#' [buzzr::commontime].
#'
#' **Time zone tip:** use the same `tz` here as you passed to
#' [buzzr::commontime] so that the axis labels match your data.
#'
#' @param tz Time zone string (e.g. `'America/New_York'`). Defaults to your
#'   system time zone. Should match the `tz` argument passed to
#'   [buzzr::commontime].
#' @return A function that accepts a POSIXct vector and returns a character
#'   vector of hour labels.
#' @seealso [buzzr::commontime] to prepare your time column,
#'   [buzzr::theme_buzzr] for a matching plot theme.
#' @examples
#' # Typical usage with ggplot2 (not run):
#' # ggplot(df, aes(x = commontime(bin_datetime, tz = 'America/New_York'),
#' #               y = detectionrate_ins_buzz, color = flower)) +
#' #   geom_line() +
#' #   scale_x_datetime(labels = label_hour(tz = 'America/New_York')) +
#' #   theme_buzzr()
#'
#' # The labeller itself:
#' fmt <- label_hour(tz = 'America/New_York')
#' fmt(as.POSIXct('2000-01-01 08:00:00', tz = 'America/New_York'))  # "8 am"
#' fmt(as.POSIXct('2000-01-01 14:00:00', tz = 'America/New_York'))  # "2 pm"
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
#' A named discrete color palette derived from the magma color scale (which is
#' also commonly used for spectrograms). Colors range from deep purple through
#' fuchsia and salmon to a warm off-white.
#'
#' Named colors: `black`, `purple_deep`, `purple`, `periwinkle`, `fuchsia`,
#' `salmon`, `tangerine`, `white_hot`.
#'
#' For a continuous magma palette see [viridis::magma].
#'
#' @examples
#' # Access named colors directly
#' palette['fuchsia']
#' palette[c('purple', 'salmon')]
#' @export
palette <- {
  palette_df <- read.csv('./resources/palette.csv')
  p <- palette_df$hex
  names(p) <- palette_df$name
  p
}


#' A ggplot2 theme for aesthetic plotting of buzzdetect results.
#'
#' Applies a clean, publication-ready style with horizontal y-axis titles,
#' a border around each panel, and suppressed minor gridlines. Available in
#' light and dark variants — the dark variant uses the deep purple from
#' [buzzr::palette] as the plot background.
#'
#' @param base_size Numeric. Base font size in points. All text elements scale
#'   relative to this value. Defaults to `10`.
#' @param mode Character. Color scheme — `'light'` (default) for a white
#'   background, `'dark'` for a deep-purple background suited to presentations
#'   or spectrograms.
#' @return A [ggplot2::theme] object that can be added to any ggplot.
#' @seealso [buzzr::label_hour] for a matching x-axis formatter,
#'   [buzzr::palette] for the buzzdetect color palette,
#'   [buzzr::commontime] to prepare time-of-day x axes.
#' @examples
#' # Light mode (default) — typical use in a detection-rate time-of-day plot:
#' # ggplot(binned, aes(x = commontime(bin_datetime, tz = 'America/New_York'),
#' #                    y = detectionrate_ins_buzz, color = flower)) +
#' #   geom_line() +
#' #   scale_x_datetime(labels = label_hour(tz = 'America/New_York')) +
#' #   labs(x = 'Time of day', y = 'Detection\nrate') +
#' #   theme_buzzr()
#'
#' # Dark mode — useful for presentations or pairing with spectrograms:
#' # ggplot(binned, aes(x = commontime(bin_datetime, tz = 'America/New_York'),
#' #                    y = detectionrate_ins_buzz, color = flower)) +
#' #   geom_line() +
#' #   scale_x_datetime(labels = label_hour(tz = 'America/New_York')) +
#' #   theme_buzzr(base_size = 14, mode = 'dark')
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
