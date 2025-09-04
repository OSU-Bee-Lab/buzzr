#' Get the start and end times for a raw result
#' @returns A data.frame with the following columns:
#'  recorder: recorder IDs
#'  timepoint: either "start" or "end", denoting whether it's the first or last frame from the recorder
#'  time: a date-time corresponding to the start or end time
#'  file: the filename or unique identifier for each file from a recorder
#'  @export
read_file_range <- function(path_raw){
  timerange <- range(read_raw(path_raw)[[COL_START_FILE]])

  df <- data.frame(
    recorder = basename(dirname(path_raw)),
    file = basename(path_raw),
    timepoint = c('start', 'end'),
    time = timerange
  )

  return(df)
}

#' Take input recorder ranges and plot out their runtime
#' @param ranges A data.frame made by \code{\link{read_file_range}}, or in the same format
#' @export
plot_recorder_ranges <- function(ranges){
  p <- ggplot(
    ranges,
    aes(
      x = time,
      y = recorder,
      group = interaction(recorder, file)
    )
  ) +

    geom_path(
      position=position_dodge(width=1),
      linewidth=1.4,
      color = 'darkblue'
    ) +
    geom_point(
      position=position_dodge(width=1),
      aes(alpha=timepoint),
      color='red',
      size=1.09,
      shape=15
    ) +

    scale_alpha_manual(values=c('start'=0, 'end'=1)) +

    theme_buzzr_light() +
    theme(legend.position = 'none')

  return(p)
}
