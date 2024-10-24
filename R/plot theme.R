palette <- read.csv('./resources/palette.csv')

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

    # axes
    #
    # text
    axis.text = ggplot2::element_text(
      size = 24,
      color = 'white'
    ),

    axis.title.y = ggplot2::element_text(
      angle=0,
      vjust=0.5,
      size = 34
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
      margin =  ggplot2::margin(b=20)
    ),

    axis.title.x = ggplot2::element_text(
      size = 38,
      margin =  ggplot2::margin(t=30, b = 30)
    )
  )
}
