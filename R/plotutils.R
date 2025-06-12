#' @export
palette <- {
  palette_df <- read.csv('./resources/palette.csv')
  p <- palette_df$hex
  names(p) <- palette_df$name
  p
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
