# Extract timestamps from paths

This function takes a vector of file paths and a vector of POSIX formats
and attempts to extract the start time of the file. See
[base::format.POSIXct](https://rdrr.io/r/base/strptime.html) for format
specifications.

## Usage

``` r
file_start_time(paths, posix_formats, tz, first_match = FALSE)
```

## Arguments

- paths:

  Character vector of file paths

- posix_formats:

  Character vector of POSIX format codes to convert the file name to a
  start time.

- tz:

  The time zone for the results, as in
  [base::as.POSIXct](https://rdrr.io/r/base/as.POSIXlt.html).

- first_match:

  If multiple formats match, should the time be returned as NA (FALSE)
  or should the first matching format be accepted (TRUE)?

## Value

POSIXct vector

## Examples

``` r
paths <- c(
  # April 6th, 2022, 1:34PM
  'foo/bar/220406_1334_buzzdetect.csv',

  # March 12th, 1924, 1:42PM
  'bar/lorem/19240312_134200_buzzdetect.csv',

  # August 25th, 2024, 8:19 AM
  'bar/lorem/20240825_081900_buzzdetect.csv'
)

# YYYYMMDD_HHMMSS vs YYMMDDHHMM; these formats will conflict with one another
posix_formats <- c('%Y%m%d_%H%M%S', '%y%m%d_%H%M')


file_start_time(paths, posix_formats, tz='America/New_York', accept_first_match=FALSE)
#> Error in file_start_time(paths, posix_formats, tz = "America/New_York",     accept_first_match = FALSE): unused argument (accept_first_match = FALSE)
# the second date will match both formats with different times, throwing a warning and returning NA
# the third date will match both formats, but the times are identical. It silently ignores the duplication.

file_start_time(paths, posix_formats, tz='America/New_York', accept_first_match=TRUE)
#> Error in file_start_time(paths, posix_formats, tz = "America/New_York",     accept_first_match = TRUE): unused argument (accept_first_match = TRUE)
# note that if we put the two-year format first, it would override the 4-year format
# and interpret the timestamp of our second file as 2024 instead of 1924!
```
