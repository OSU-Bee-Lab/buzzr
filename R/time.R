#' Coerce all dates to the same day, preserving time of day
#'
#' For all input times, the date will be coerced to 2000-01-01 and the time zone of choice (defaults to your current time zone).
#' This makes for prettier graphing than using time of day as a numeric (e.g., allows ggplot2::scale_x_datetime)
#' and allows plotting data from different days on a common X axis.
#'
#'
#' @param times `r DOC_PARAM_TIMES`
#' @export
commontime <- function(times, tz=Sys.timezone()) {
  lubridate::date(times) <- '2000-01-01'
  lubridate::tz(times) <- tz
  return(times)
}

#' Get the time of day as a 0-1 value (left-inclusive) where 0 represents midnight
#'
#' @param times `r DOC_PARAM_TIMES`
#' @export
time_of_day <- function(times){
  h <- lubridate::hour(times)
  m <- lubridate::minute(times)
  s <- lubridate::second(times)

  tod <- (h/24) + (m/(60*24)) + (s/(60*60*24))

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

#' Extract timestamps from paths
#'
#' This function takes a vector of file paths and a vector of POSIX formats
#' and attempts to extract the start time of the file
#'
#' @param paths Character vector of file paths
#' @param posix_formats Character vector of posix_formats to try. See [base::format.POSIXct].
#' @param tz What time zone should the timestamp be interpreted as? See [base::timezones]
#' @param accept_first_match If multiple formats match, should the time be returned as NA (FALSE) or should the first matching format be accepted (TRUE)?
#' @return POSIXct vector
#' @importFrom magrittr %>%
#' @example examples/file_start_time.R
#'
#' @export
file_start_time <- function(paths, posix_formats, tz, accept_first_match=FALSE){
  regex_patterns <- sapply(posix_formats, posix_to_regex, USE.NAMES=T)

  time_from_path <- function(path){
    filename <- path %>%
      tools::file_path_sans_ext() %>%
      basename()

    time_matches <- lapply(
      posix_formats,
      FUN = function(posix_format) {
        filename %>%
          stringr::str_extract(regex_patterns[[posix_format]]) %>%
          as.POSIXct(format = posix_format, tz=tz)
      }
    ) %>%
      # unlist, without coercing to integer
      do.call(c, .)

    names(time_matches) <- posix_formats


    # in case multiple formats resolve to the same time,
    # which shouldn't throw an error
    times_unique <- unique(time_matches[!is.na(time_matches)])

    if(length(times_unique) == 0){
      warning('Returning NA for start time of file \'', path, '\'. No POSIX matches found.', call.=F)
      return(as.POSIXct(NA))
    }

    if(length(times_unique) > 1){
      collisions <- time_matches[time_matches %in% times_unique] %>%
        names()

      if(accept_first_match){
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
  ) %>%
    do.call(c,.)

  return(times)
}
