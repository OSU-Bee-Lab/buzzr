# Bin results by time, summing detections and frames per bin.

Groups frame-level results into fixed-width time bins and sums all
`detections_` columns and the `frames` column within each bin. Any
remaining columns (e.g. from `dir_nesting`) are used as grouping
variables, so bins are computed separately for each unique combination
of those columns.

## Usage

``` r
bin(results, binwidth, calculate_rate = F)
```

## Arguments

- results:

  A data frame or data.table of buzzdetect results, such as created by
  [read_results](https://osu-bee-lab.github.io/buzzr/reference/read_results.md)..
  Must contain at least one time column (`start_filetime`,
  `start_datetime`, `bin_filetime`, or `bin_datetime`) and at least one
  `detections_` column.

- binwidth:

  Width of each time bin in minutes (e.g. `5`, `20`, or `60`).

- calculate_rate:

  If `TRUE`, adds a `detectionrate_` column for each `detections_`
  column, calculated as detections divided by frames. Values range from
  0 to 1.

## Value

A data.table with a `bin_filetime` or `bin_datetime` column, summed
`detections_` columns, a `frames` column, and optionally
`detectionrate_` columns. All other input columns are retained as
grouping variables.

## Details

Can also re-bin previously binned results. For cleanest results, choose
a new `binwidth` that is a multiple of the original (e.g. re-bin
5-minute bins into 60-minute bins). Non-multiple re-binning is allowed
but produces boundary artefacts.

Detection rates (`detectionrate_` columns) are best calculated at the
final binning step. If you plan to re-bin, leave
`calculate_rate = FALSE` until then.

## See also

[call_detections](https://osu-bee-lab.github.io/buzzr/reference/call_detections.md)
to produce the `detections_` columns that this function expects,
[frames_expected](https://osu-bee-lab.github.io/buzzr/reference/frames_expected.md)
to check whether your bins contain the right number of frames.

## Examples

``` r
path <- system.file(
  'extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv',
  package = 'buzzr'
)
results <- read_results(path, posix_formats = '%y%m%d_%H%M', tz = 'America/New_York')
called  <- call_detections(results, thresholds = c(ins_buzz = -1.2))

# 15-minute bins
bin(called, binwidth = 15)
#>            bin_datetime detections_ins_buzz frames
#>                  <POSc>               <int>  <num>
#>  1: 2023-08-09 00:00:00                   0    938
#>  2: 2023-08-09 00:15:00                   1    937
#>  3: 2023-08-09 00:30:00                   0    938
#>  4: 2023-08-09 00:45:00                   0    937
#>  5: 2023-08-09 01:00:00                   0    938
#>  6: 2023-08-09 01:15:00                   0    937
#>  7: 2023-08-09 01:30:00                   0    938
#>  8: 2023-08-09 01:45:00                   0    937
#>  9: 2023-08-09 02:00:00                   0    938
#> 10: 2023-08-09 02:15:00                   0    937
#> 11: 2023-08-09 02:30:00                   0    938
#> 12: 2023-08-09 02:45:00                   0    937
#> 13: 2023-08-09 03:00:00                   0    938
#> 14: 2023-08-09 03:15:00                   0    937
#> 15: 2023-08-09 03:30:00                   0    938
#> 16: 2023-08-09 03:45:00                   0    937
#> 17: 2023-08-09 04:00:00                   0    938
#> 18: 2023-08-09 04:15:00                   4    937
#> 19: 2023-08-09 04:30:00                   0    938
#> 20: 2023-08-09 04:45:00                   0    937
#> 21: 2023-08-09 05:00:00                   0    938
#> 22: 2023-08-09 05:15:00                   0    937
#> 23: 2023-08-09 05:30:00                   0    938
#> 24: 2023-08-09 05:45:00                   0    937
#> 25: 2023-08-09 06:00:00                   0    938
#> 26: 2023-08-09 06:15:00                   0    937
#> 27: 2023-08-09 06:30:00                   0    938
#> 28: 2023-08-09 06:45:00                   1    937
#> 29: 2023-08-09 07:00:00                   2    938
#> 30: 2023-08-09 07:15:00                   4    937
#> 31: 2023-08-09 07:30:00                  12    938
#> 32: 2023-08-09 07:45:00                   9    937
#> 33: 2023-08-09 08:00:00                   4    938
#> 34: 2023-08-09 08:15:00                   3    937
#> 35: 2023-08-09 08:30:00                   5    938
#> 36: 2023-08-09 08:45:00                   4    937
#> 37: 2023-08-09 09:00:00                   5    938
#> 38: 2023-08-09 09:15:00                   0    937
#> 39: 2023-08-09 09:30:00                   7    938
#> 40: 2023-08-09 09:45:00                  25    937
#> 41: 2023-08-09 10:00:00                  20    938
#> 42: 2023-08-09 10:15:00                  28    937
#> 43: 2023-08-09 10:30:00                  30    938
#> 44: 2023-08-09 10:45:00                 122    937
#> 45: 2023-08-09 11:00:00                 433    938
#> 46: 2023-08-09 11:15:00                 164    937
#> 47: 2023-08-09 11:30:00                 533    938
#> 48: 2023-08-09 11:45:00                 508    937
#> 49: 2023-08-09 12:00:00                 502    938
#> 50: 2023-08-09 12:15:00                 588    937
#> 51: 2023-08-09 12:30:00                 587    938
#> 52: 2023-08-09 12:45:00                 469    937
#> 53: 2023-08-09 13:00:00                 714    938
#> 54: 2023-08-09 13:15:00                 677    937
#> 55: 2023-08-09 13:30:00                 705    938
#> 56: 2023-08-09 13:45:00                 624    937
#> 57: 2023-08-09 14:00:00                 413    938
#> 58: 2023-08-09 14:15:00                 563    937
#> 59: 2023-08-09 14:30:00                 321    938
#> 60: 2023-08-09 14:45:00                  92    937
#> 61: 2023-08-09 15:00:00                  10    938
#> 62: 2023-08-09 15:15:00                  23    937
#> 63: 2023-08-09 15:30:00                 203    938
#> 64: 2023-08-09 15:45:00                  53    937
#> 65: 2023-08-09 16:00:00                  57    938
#> 66: 2023-08-09 16:15:00                  83    937
#> 67: 2023-08-09 16:30:00                 100    938
#> 68: 2023-08-09 16:45:00                   4    937
#> 69: 2023-08-09 17:00:00                  11    938
#> 70: 2023-08-09 17:15:00                  17    937
#> 71: 2023-08-09 17:30:00                   0    938
#> 72: 2023-08-09 17:45:00                   2    937
#> 73: 2023-08-09 18:00:00                   1    938
#> 74: 2023-08-09 18:15:00                   0    937
#> 75: 2023-08-09 18:30:00                   0    938
#> 76: 2023-08-09 18:45:00                   0    937
#> 77: 2023-08-09 19:00:00                   3    938
#> 78: 2023-08-09 19:15:00                   0    937
#> 79: 2023-08-09 19:30:00                   1    938
#> 80: 2023-08-09 19:45:00                   2    937
#> 81: 2023-08-09 20:00:00                   1    938
#> 82: 2023-08-09 20:15:00                   0    937
#> 83: 2023-08-09 20:30:00                   0    938
#> 84: 2023-08-09 20:45:00                   0    937
#> 85: 2023-08-09 21:00:00                   1    938
#> 86: 2023-08-09 21:15:00                   0    937
#> 87: 2023-08-09 21:30:00                   0    938
#> 88: 2023-08-09 21:45:00                   0    937
#> 89: 2023-08-09 22:00:00                   0    938
#> 90: 2023-08-09 22:15:00                   0    937
#> 91: 2023-08-09 22:30:00                   0    938
#> 92: 2023-08-09 22:45:00                   0    937
#> 93: 2023-08-09 23:00:00                   0    938
#> 94: 2023-08-09 23:15:00                   0    937
#> 95: 2023-08-09 23:30:00                   0    938
#> 96: 2023-08-09 23:45:00                   0    937
#>            bin_datetime detections_ins_buzz frames
#>                  <POSc>               <int>  <num>

# With detection rates
binned <- bin(called, binwidth = 15, calculate_rate = TRUE)

# Re-bin to 1-hour windows for a coarser view
bin(binned, binwidth = 60)
#>            bin_datetime detections_ins_buzz frames
#>                  <POSc>               <int>  <num>
#>  1: 2023-08-09 00:00:00                   1   3750
#>  2: 2023-08-09 01:00:00                   0   3750
#>  3: 2023-08-09 02:00:00                   0   3750
#>  4: 2023-08-09 03:00:00                   0   3750
#>  5: 2023-08-09 04:00:00                   4   3750
#>  6: 2023-08-09 05:00:00                   0   3750
#>  7: 2023-08-09 06:00:00                   1   3750
#>  8: 2023-08-09 07:00:00                  27   3750
#>  9: 2023-08-09 08:00:00                  16   3750
#> 10: 2023-08-09 09:00:00                  37   3750
#> 11: 2023-08-09 10:00:00                 200   3750
#> 12: 2023-08-09 11:00:00                1638   3750
#> 13: 2023-08-09 12:00:00                2146   3750
#> 14: 2023-08-09 13:00:00                2720   3750
#> 15: 2023-08-09 14:00:00                1389   3750
#> 16: 2023-08-09 15:00:00                 289   3750
#> 17: 2023-08-09 16:00:00                 244   3750
#> 18: 2023-08-09 17:00:00                  30   3750
#> 19: 2023-08-09 18:00:00                   1   3750
#> 20: 2023-08-09 19:00:00                   6   3750
#> 21: 2023-08-09 20:00:00                   1   3750
#> 22: 2023-08-09 21:00:00                   1   3750
#> 23: 2023-08-09 22:00:00                   0   3750
#> 24: 2023-08-09 23:00:00                   0   3750
#>            bin_datetime detections_ins_buzz frames
#>                  <POSc>               <int>  <num>
```
