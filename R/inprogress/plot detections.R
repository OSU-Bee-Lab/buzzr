#
# Imports and setup ----
#
library(dplyr)
library(lubridate)
library(ggplot2)


cp_df <- read.csv('./resources/palette.csv')
pallette <- cp_df$hex
names(pallette) <- cp_df$name

theme_buzzdetect <- ggplot2::theme_light() +
  ggplot2::theme(
    # text
    #
    text = element_text(
      size=26,
      family="Futura-Medium",
      color = 'white'
    ),

    # panel
    #
    plot.background = element_rect( # oh...right, I put the panel on top. Will have to do in GIMP
      fill = pallette['purple_deep']
    ),

    # axes
    #
    # text
    axis.text = element_text(
      size = 24,
      color = 'white'
    ),

    axis.title.y = element_text(
      angle=0,
      vjust=0.5,
      size = 34
    ),

    # grid
    panel.grid.major.x = element_line(
      color = "darkgray",
      linewidth = 0.8
    ),

    panel.grid.minor.x = element_line(linewidth = 0),
    panel.grid.major.y = element_line(linewidth = 0.2, color = 'white'),
    panel.grid.minor.y = element_line(linewidth = 0),
    panel.ontop = F,

    plot.margin = margin(2,3,0,1, 'cm'),

    strip.text = element_text(
      size = 24,
      color = 'white'
    )
  )


warning('time of day plotting current demands a color and frankly I can\'t be bothered to fix that right now')
detections_timeofday <- function(detections, color_var = NULL, row_var = NULL, col_var=NULL){
  binwidth <- detections$start_bin %>%
    sort() %>%
    unique() %>%
    {difftime(.[2], .[1], units = 'secs')} %>%
    as.numeric()

  detections_plot <- detections %>%
    mutate(time_of_day = (hour(start_bin)) + (minute(start_bin)/60)) %>%
    group_by(across(-c(detections, frames_total))) %>%
    summarize(
      detections = sum(detections),
      frames_total = sum(frames_total)
    ) %>%
    mutate(
      detectionrate = detections/frames_total,
      time = as.POSIXct('2024-01-01') + (time_of_day*60*60)
    )

  plot <- ggplot(
    bind_rows(
      detections_plot,
      detections_plot %>%
        mutate(time = time+(0.99*binwidth))
    ),
    aes(
      x = time,
      y = detectionrate
    )
  ) +
    geom_area(
      alpha = 0.4,
      linewidth=1 # will need fine-tuning to graph
    ) +

    # theme edits
    #
    ylab(paste0("detection \n      rate")) +
    xlab('') +
    theme_minimal() +
    scale_x_datetime(
      breaks = '4 hours',
      labels = function(breaks){format(breaks, format = "%-I:%M %p")}  # NOTE! %-I removes leading 0 only on Unix; for windows use #
    ) +
    theme_buzzdetect
}

detections_timeofday(detections)
