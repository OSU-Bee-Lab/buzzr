#' Calculate the expected number of frames in a time bin.
#'
#' A convenience function for checking whether a binned results data.table has
#' the right number of frames. Frames per bin = `(binwidth_min * 60) / framelength_s`.
#' Compare against the `frames` column of a binned result to identify bins with
#' missing audio (e.g. recorder dropouts or files shorter than the bin width).
#'
#' @param binwidth_min Numeric. Bin width in minutes.
#' @param framelength_s Numeric. Frame length in seconds.
#'   For `model_general_v3` this is `0.96`.
#' @return A single numeric value: the expected number of frames per full bin.
#' @examples
#' # model_general_v3 uses 0.96-second frames
#' frames_expected(binwidth_min = 5,  framelength_s = 0.96)  # 312.5
#' frames_expected(binwidth_min = 20, framelength_s = 0.96)  # 1250
#' frames_expected(binwidth_min = 60, framelength_s = 0.96)  # 3750
#'
#' # Check for bins with incomplete audio coverage
#' \dontrun{
#' binned <- bin_directory(dir, thresholds = c(ins_buzz = -1.2), binwidth = 20)
#' expected <- frames_expected(20, 0.96)
#' binned[frames < expected * 0.9]  # bins with > 10% missing audio
#' }
#' @export
frames_expected <- function(binwidth_min, framelength_s){
  (binwidth_min*60)/framelength_s
}

regex_timestamp <- '^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}'
get_timestamp <- function(logitems){
  logitems |>
    stringr::str_extract(regex_timestamp) |>
    as.POSIXct()
}

get_duration <- function(logitems){
  if(length(logitems)==1){
    return(0)
  } else if(length(logitems) > 2){
    # we know log is sequential, just take first and last item
    logitems <- logitems[c(1, length(logitems))]
  }

  logitems |>
    get_timestamp() |>
    as.POSIXct() |>
    diff() |>
    as.numeric(units='secs')
}

#' Summarize a buzzdetect log file
#'
#' Evaluate performance in a buzzdetect analysis log. Reports:
#' * duration_total - how many seconds did the analysis take?
#' * duration_audio - how many seconds of audio were analyzed?
#' * analysis_rate - how many seconds of audio were analyzed per second of wall time? NOTE! This does NOT use duration_total as the divisor, it starts counting when the first chunk is reported.
#' * bottleneck_time - how long did analyzers wait for streamers to fill the queue?
#' * duration_warmup - how long from the launching of analysis until the first chunk was analyzed? Includes results checking and cleaning, streamer launching, etc.
#'
#' Useful for tuning analysis settings.
#'
#'
#' @param path_log String. The path to a buzzdetect results log.
#' @return A named vector of analysis metrics
#' @export
evaluate_log <- function(path_log){
  logitems <- readLines(path_log)

  reports_timestamped <- logitems[grepl(regex_timestamp, logitems, perl=T)]

  duration_total <- get_duration(reports_timestamped)

  # Analysis rate
  #
  reports_analyzed <- logitems[grepl('(rate: .*)', logitems, perl=T)]
  seconds_audio <-  reports_analyzed |>
    stringr::str_extract(pattern = 'chunk \\((.*)\\) in', group=1) |>
    stringr::str_extract(pattern='(.*), (.*)', group = c(1,2)) |>
    apply(2, as.numeric) |>
    apply(1, diff)

  if(length(reports_analyzed) > 1){

    # this is the duration *between* the first and last analysis, and so excludes the time that
    # the first analyzer started
    duration_analysis <- get_duration(reports_analyzed)

    # drop the first chunk, since it wasn't included in duration_analysis
    analysis_rate <- sum(seconds_audio[c(2:length(seconds_audio))])/duration_analysis
  } else {
    # if we only have one report, use that report
    analysis_rate <- reports_analyzed |>
      stringr::str_extract('rate\\: (.*)\\)', group=1) |>
      as.numeric()
  }


  # Warmup - wall time from launch to first analysis
  #
  duration_warmup <- c(reports_timestamped[1], reports_analyzed[1]) |>
    get_duration()

  # Bottlenecks
  #
  reports_bottleneck <- logitems[grepl(stringr::fixed('BUFFER BOTTLENECK:'), logitems, perl=T)]

  seconds_bottleneck <- reports_bottleneck |>
    stringr::str_extract(pattern='received assignment after (\\d+\\.\\d+)s', group=1) |>
    as.numeric() |>
    sum()


  logreport <- c(
    duration_total = duration_total,
    duration_audio = sum(seconds_audio),
    analysis_rate = analysis_rate,
    bottleneck_time = seconds_bottleneck,
    duration_warmup = duration_warmup
  )

  return(logreport)
}
