# Read, threshold, and bin all buzzdetect result files in a directory.

A convenience wrapper that runs
[read_directory](https://osu-bee-lab.github.io/buzzr/reference/read_directory.md),
[call_detections](https://osu-bee-lab.github.io/buzzr/reference/call_detections.md),
and [bin](https://osu-bee-lab.github.io/buzzr/reference/bin.md) in
sequence. All arguments from those functions are accepted here. See
their documentation for details.

## Usage

``` r
bin_directory(
  dir_results,
  thresholds,
  posix_formats = NA,
  first_match = FALSE,
  drop_filetime = TRUE,
  dir_nesting = NULL,
  return_ident = FALSE,
  tz = NA,
  binwidth = 5,
  calculate_rate = FALSE,
  workers = 2
)
```

## Arguments

- dir_results:

  The directory holding all buzzdetect results to be read.

- thresholds:

  A named numeric vector mapping neuron names to detection thresholds
  (e.g. `c(ins_buzz = -1.2)`). Frames whose activation value *exceeds*
  the threshold are counted as detections. Because `model_general_v3`
  outputs negative log-likelihoods, thresholds are typically negative.
  Activations *above* the threshold value are counted as detections, so
  thresholds for models that output negative log-likelihoods (such as
  `model_general_v3`) are typically negative (e.g. `-1.2`).

- posix_formats:

  Character vector of POSIX format strings (see
  [base::strptime](https://rdrr.io/r/base/strptime.html)) describing the
  timestamp embedded in each file name (e.g. `'%y%m%d_%H%M'` for
  `230809_0600`). Supply multiple strings when recordings from different
  logger types are mixed in one directory. If `NA` (default), results
  are left in file-time. See
  [file_start_time](https://osu-bee-lab.github.io/buzzr/reference/file_start_time.md).

- first_match:

  Controls behaviour when multiple formats produce *different* times for
  the same file. `FALSE` (default) returns `NA` with a warning; `TRUE`
  accepts the time from the first matching format with a message.

- drop_filetime:

  If `TRUE` (default), the `start_filetime` / `bin_filetime` column is
  removed once `start_datetime` / `bin_datetime` has been added. Set
  `FALSE` to keep both. Ignored if no POSIX formats are given.

- dir_nesting:

  A character vector used to name the directory levels above the results
  file. Each element becomes a column in the output, storing the
  components of the path. For example, dir_nesting = c('site',
  'recorder') for the path
  data/2026/siteA/recorder_4/250704_11343_buzzdetect.csv would add a
  site column (holding the value 'siteA') and a recorder column (holding
  the value 'recorder_4').

- return_ident:

  The 'ident' is the relative path from your data directory to the
  results file, without the \_buzzdetect tag or file extension. Useful
  for finding corresponding audio files or annotations. Set `TRUE` to
  add an `ident` column as the first column of the output.

- tz:

  Time zone string passed to
  [base::as.POSIXct](https://rdrr.io/r/base/as.POSIXlt.html) (e.g.
  `'America/New_York'`). See
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html) for valid
  values.. Ignored if the results already contain a `start_datetime` or
  `bin_datetime` column.

- binwidth:

  Width of each time bin in minutes (e.g. `5`, `20`, or `60`).

- calculate_rate:

  If `TRUE`, adds a `detectionrate_` column for each `detections_`
  column, calculated as detections divided by frames. Values range from
  0 to 1.

- workers:

  Number of parallel workers to use when processing results. Set to
  `Inf` to use all available cores. Note that because data.table already
  uses multiple threads, you may want to set fewer workers than there
  are cores on your machine. Overridden by MC_CORES environmental
  variable if set.

## Value

A data.table with `bin_filetime` or `bin_datetime`, `detections_`
columns, a `frames` column, and any columns from `dir_nesting` or
`ident`.

## Details

This is the recommended entry point for most analyses: point it at your
results folder, supply your thresholds and time zone, and get back a
tidy binned data.table ready for plotting or modelling.

## See also

[read_directory](https://osu-bee-lab.github.io/buzzr/reference/read_directory.md),
[call_detections](https://osu-bee-lab.github.io/buzzr/reference/call_detections.md),
and [bin](https://osu-bee-lab.github.io/buzzr/reference/bin.md) for the
individual steps this function wraps,
[frames_expected](https://osu-bee-lab.github.io/buzzr/reference/frames_expected.md)
to check bins for missing audio coverage.

## Examples

``` r
dir <- system.file('extdata/five_flowers', package = 'buzzr')

bin_directory(
  dir_results    = dir,
  thresholds     = c(ins_buzz = -1.2),
  posix_formats  = '%y%m%d_%H%M',
  tz             = 'America/New_York',
  dir_nesting    = c('flower', 'recorder'),
  binwidth       = 20,
  calculate_rate = TRUE
)
#>          flower recorder        bin_datetime detections_ins_buzz frames
#>          <char>   <char>              <POSc>               <int>  <num>
#>   1:    chicory    1_104 2025-07-04 00:00:00                   1   1250
#>   2:    chicory    1_104 2025-07-04 00:20:00                   4   1250
#>   3:    chicory    1_104 2025-07-04 00:40:00                   3   1250
#>   4:    chicory    1_104 2025-07-04 01:00:00                   2   1250
#>   5:    chicory    1_104 2025-07-04 01:20:00                   0   1250
#>  ---                                                                   
#> 356: watermelon     1_73 2024-07-27 22:20:00                   1   1250
#> 357: watermelon     1_73 2024-07-27 22:40:00                   0   1250
#> 358: watermelon     1_73 2024-07-27 23:00:00                   2   1250
#> 359: watermelon     1_73 2024-07-27 23:20:00                   1   1250
#> 360: watermelon     1_73 2024-07-27 23:40:00                   0   1250
#>      detectionrate_ins_buzz
#>                       <num>
#>   1:                 0.0008
#>   2:                 0.0032
#>   3:                 0.0024
#>   4:                 0.0016
#>   5:                 0.0000
#>  ---                       
#> 356:                 0.0008
#> 357:                 0.0000
#> 358:                 0.0016
#> 359:                 0.0008
#> 360:                 0.0000
```
