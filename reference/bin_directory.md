# Read and bin all buzzdetect results in a directory

Serves as a one-step implementation of
[read_directory](https://osu-bee-lab.github.io/buzzr/reference/read_directory.md),
[call_detections](https://osu-bee-lab.github.io/buzzr/reference/call_detections.md),
and [bin](https://osu-bee-lab.github.io/buzzr/reference/bin.md). See
documentation of these functions for details.

## Usage

``` r
bin_directory(
  dir_results,
  thresholds,
  posix_formats = NA,
  first_match = FALSE,
  drop_filetime = TRUE,
  dir_nesting = NULL,
  return_filename = FALSE,
  tz = NA,
  binwidth = 5,
  calculate_rate = FALSE
)
```

## Arguments

- dir_results:

  The directory holding all buzzdetect results to be read.

- thresholds:

  A named numeric vector with names corresponding to neuron names and
  values corresponding to the desired detection threshold for that
  neuron.

- posix_formats:

  Character vector of POSIX format codes to convert the file name to a
  start time. If NA, leaves results in file time. See
  [file_start_time](https://osu-bee-lab.github.io/buzzr/reference/file_start_time.md).

- first_match:

  If multiple formats match, should the time be returned as NA (FALSE)
  or should the first matching format be accepted (TRUE)?

- drop_filetime:

  Should the column storing the file time (start_filetime or
  bin_filetime) be removed from the results? Ignored if no POSIX formats
  are given.

- dir_nesting:

  A character vector used to name the directory levels above the results
  file. Each element becomes a column in the output, storing the
  components of the path. For example, dir_nesting = c('site',
  'recorder') for the path
  data/2026/siteA/recorder_4/250704_11343_buzzdetect.csv would add a
  site column (holding the value 'siteA') and a recorder column (holding
  the value 'recorder_4').

- return_filename:

  A boolean representing whether or not to return the filename as an
  element/column.

- tz:

  The time zone for the results, as in
  [base::as.POSIXct](https://rdrr.io/r/base/as.POSIXlt.html).. If the
  results already have a start_datetime or bin_datetime column, this
  will be ignored.

- binwidth:

  The desired width of the bin in minutes

## Value

A data.table
