#' Coerce all dates to the same day, preserving time of day.
#'
#' Sets the date of every element in `times` to 2000-01-01 while keeping the
#' time-of-day unchanged. This lets you overlay recordings from different
#' calendar days on a shared x axis using [ggplot2::scale_x_datetime] and
#' [buzzr::label_hour]. The arbitrary date 2000-01-01 was chosen because it
#' is far enough from DST transition dates to avoid edge cases in most time zones.
#'
#' **Time zone tip:** pass the same `tz` here as you pass to
#' [buzzr::label_hour] to ensure the hour labels match the data.
#'
#' @param times `r DOC_PARAM_TIMES`
#' @param tz Time zone string (e.g. `'America/New_York'`). Defaults to your
#'   system time zone. Should match the `tz` argument passed to
#'   [buzzr::label_hour].
#' @return A POSIXct vector of the same length as `times`, with the date
#'   component set to 2000-01-01.
#' @seealso [buzzr::label_hour] for the matching x-axis label formatter,
#'   [buzzr::time_of_day] for a numeric alternative suitable for modelling.
#' @examples
#' times <- as.POSIXct(
#'   c('2023-08-09 06:15:00', '2024-07-27 14:30:00'),
#'   tz = 'America/New_York'
#' )
#'
#' # Both dates become 2000-01-01; times of day are preserved
#' commontime(times, tz = 'America/New_York')
#'
#' # Typical ggplot2 usage (not run here):
#' # ggplot(df, aes(x = commontime(bin_datetime, tz = 'America/New_York'),
#' #               y = detectionrate_ins_buzz, color = flower)) +
#' #   geom_line() +
#' #   scale_x_datetime(labels = label_hour(tz = 'America/New_York'))
#' @export
commontime <- function(times, tz=Sys.timezone()) {
  lubridate::date(times) <- '2000-01-01'
  lubridate::tz(times) <- tz
  return(times)
}

#' Convert a date-time to a numeric time of day.
#'
#' Returns the time of day as either a proportion of the 24-hour day
#' (`0` = midnight, `0.5` = noon, `1` = next midnight) or as a decimal hour
#' (`0` = midnight, `6.5` = 6:30 AM, `23.5` = 11:30 PM). Seconds are included
#' in the calculation.
#'
#' This is useful for statistical analyses — for example, fitting a circular
#' model of daily activity or finding the hour of peak buzz detections. For
#' plotting, use [buzzr::commontime] and [buzzr::label_hour] instead.
#'
#' @param times `r DOC_PARAM_TIMES`
#' @param time_format One of `'proportion'` (default) or `'hour'`.
#' @return A numeric vector of the same length as `times`.
#' @seealso [buzzr::commontime] for a POSIXct alternative suitable for plotting.
#' @examples
#' times <- as.POSIXct(
#'   c('2023-08-09 00:00:00', '2023-08-09 06:00:00', '2023-08-09 18:30:00'),
#'   tz = 'America/New_York'
#' )
#'
#' # Midnight = 0, 6 AM = 0.25, noon = 0.5
#' time_of_day(times)
#'
#' # As decimal hours: midnight = 0, 6 AM = 6, 6:30 PM = 18.5
#' time_of_day(times, time_format = 'hour')
#' @export
time_of_day <- function(times, time_format='proportion'){
  if(!(time_format %in% c('proportion', 'hour'))){
    stop('time_format must be either proportion or hour')
  }

  h <- lubridate::hour(times)
  m <- lubridate::minute(times)
  s <- lubridate::second(times)

  tod <- (h/24) + (m/(60*24)) + (s/(60*60*24))

  if(time_format == 'hour'){
    tod <- tod*24
  }

  return(tod)
}

posix_to_regex <- function(posix_format) {
  # split format
  parts_posix <- stringr::str_extract_all(posix_format, "%[a-zA-Z]|[^%]+")[[1]]

  # use vector indexing to map tokens
  parts_regex <- conversion_specs[parts_posix]

  # Identify literals (NAs in the lookup) and escape them automatically
  literals <- is.na(parts_regex)
  parts_regex[literals] <- stringr::str_escape(parts_posix[literals])

  regex_pattern <- paste0(parts_regex, collapse='')
  # return collapsed regex
  return(regex_pattern)
}

# unlist coerces to an integer
unlist_posix <- function(posixlist){
  # but do.call freaks if you have one element
  if(length(posixlist) > 1){
    posixlist <- do.call(c, posixlist)
  } else {posixlist <- posixlist[[1]]}

  return(posixlist)
}

#' Extract date time information from a file's name.
#'
#' Extract recording start date-times from file names.
#'
#' Parses date-time information embedded in file names using one or more POSIX
#' format strings (see [base::strptime] for format codes). The file extension
#' and `_buzzdetect` suffix are stripped before matching, so the format string
#' should describe only the timestamp portion of the base name.
#'
#' When a single format unambiguously matches, its parsed time is returned.
#' When multiple formats match the same file *with the same result*, the
#' duplicate is silently ignored. When they produce *different* times, the
#' behaviour depends on `first_match`:
#' - `FALSE` (default): returns `NA` with a warning listing the conflicting formats.
#' - `TRUE`: returns the time from the first matching format with a message.
#'
#' If no format matches, `NA` is returned with a warning.
#'
#' @param paths Character vector of file paths.
#' @param posix_formats `r DOC_PARAM_POSIX_FORMATS`
#'   See [base::strptime] for format codes.
#' @param tz `r DOC_PARAM_TZ`
#' @param first_match `r DOC_PARAM_FIRST_MATCH`
#' @return A POSIXct vector the same length as `paths`.
#' @seealso [buzzr::read_results] which calls this internally when
#'   `posix_formats` is supplied.
#' @examples
#' # Single file, single format (YYMMDD_HHMM)
#' file_start_time(
#'   'soybean/9/230809_0000_buzzdetect.csv',
#'   posix_formats = '%y%m%d_%H%M',
#'   tz = 'America/New_York'
#' )
#'
#' # Multiple files in one call
#' paths <- c(
#'   'soybean/9/230809_0000_buzzdetect.csv',
#'   'chicory/1_104/250704_0000_buzzdetect.csv'
#' )
#' file_start_time(paths, posix_formats = '%y%m%d_%H%M', tz = 'America/New_York')
#'
#' # Two formats for two recorder types in the same experiment;
#' # first_match = FALSE returns NA (with a warning) if both match a file
#' file_start_time(
#'   paths,
#'   posix_formats = c('%y%m%d_%H%M', '%Y%m%d_%H%M%S'),
#'   tz = 'America/New_York',
#'   first_match = FALSE
#' )
#' @export
file_start_time <- function(paths, posix_formats, tz, first_match=FALSE){
  regex_patterns <- sapply(posix_formats, posix_to_regex, USE.NAMES=T)

  time_from_path <- function(path){
    filename <- path |>
      tools::file_path_sans_ext() |>
      basename()

    time_matches <- lapply(
      posix_formats,
      FUN = function(posix_format) {
        filename |>
          stringr::str_extract(regex_patterns[[posix_format]]) |>
          as.POSIXct(format = posix_format, tz=tz)
      }
    ) |>
      unlist_posix()


    names(time_matches) <- posix_formats


    # in case multiple formats resolve to the same time,
    # which shouldn't throw an error
    times_unique <- unique(time_matches[!is.na(time_matches)])

    if(length(times_unique) == 0){
      warning('Returning NA for start time of file \'', path, '\'. No POSIX matches found.', call.=F)
      return(as.POSIXct(NA))
    }

    if(length(times_unique) > 1){
      collisions <- time_matches[time_matches %in% times_unique] |>
        names()

      if(first_match){
        message('Multiple POSIXct patterns match for file \'', path, '\'. Accepting first: ', collisions[1])
        return(times_unique[[1]])
      } else {
        warning('Returning NA for start time of file \'', path, '\'; multiple POSIX matches: \n', paste(collisions, collapse='\n'), call.=F)
        return(as.POSIXct(NA))
      }
    }

    return(times_unique)
  }


  times <- lapply(
    paths,
    time_from_path
  ) |>
    unlist_posix()

  return(times)
}
