# Read buzzdetect results

This function can read in results at any stage of analysis. File formats
can be .csv or .rds. File contents can be raw buzzdetect results (.csv
with a "start" column and a column for each neuron activation), unbinned
results (a row for each frame, start columns as start_datetime and/or
start_filetime, and results columns for activations or for detections),
or binned results (a row for each bin, start columns as bin_datetime
and/or bin_filetime, and results columns for detections). The results
file can be the results from one audio file or merged results (as
created by buzzr:read_directory or buzzr:bin_directory).

## Usage

``` r
read_results(
  path_results,
  posix_formats = NA,
  first_match = FALSE,
  drop_filetime = TRUE,
  tz = NA,
  dir_nesting = NULL,
  return_filename = FALSE
)
```

## Arguments

- path_results:

  The path to any buzzdetect result file that can be read with
  read_results.

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

- tz:

  The time zone for the results, as in
  [base::as.POSIXct](https://rdrr.io/r/base/as.POSIXlt.html).. If the
  results already have a start_datetime or bin_datetime column, this
  will be ignored.

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

## Details

NOTE: "start" columns from raw buzzdetect files will be renamed to
"start_filetime". This enables buzzr to discriminate between frames and
bins, file-times and real-world-times.
