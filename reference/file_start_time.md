# Extract date time information from a file's name.

Extract recording start date-times from file names.

## Usage

``` r
file_start_time(paths, posix_formats, tz, first_match = FALSE)
```

## Arguments

- paths:

  Character vector of file paths.

- posix_formats:

  Character vector of POSIX format strings (see
  [base::strptime](https://rdrr.io/r/base/strptime.html)) describing the
  timestamp embedded in each file name (e.g. `'%y%m%d_%H%M'` for
  `230809_0600`). Supply multiple strings when recordings from different
  logger types are mixed in one directory. See
  [base::strptime](https://rdrr.io/r/base/strptime.html) for format
  codes.

- tz:

  Time zone string passed to
  [base::as.POSIXct](https://rdrr.io/r/base/as.POSIXlt.html) (e.g.
  `'America/New_York'`). See
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html) for valid
  values.

- first_match:

  Controls behaviour when multiple formats produce *different* times for
  the same file. `FALSE` (default) returns `NA` with a warning; `TRUE`
  accepts the time from the first matching format with a message.

## Value

A POSIXct vector the same length as `paths`.

## Details

Parses date-time information embedded in file names using one or more
POSIX format strings (see
[base::strptime](https://rdrr.io/r/base/strptime.html) for format
codes). The file extension and `_buzzdetect` suffix are stripped before
matching, so the format string should describe only the timestamp
portion of the base name.

When a single format unambiguously matches, its parsed time is returned.
When multiple formats match the same file *with the same result*, the
duplicate is silently ignored. When they produce *different* times, the
behaviour depends on `first_match`:

- `FALSE` (default): returns `NA` with a warning listing the conflicting
  formats.

- `TRUE`: returns the time from the first matching format with a
  message.

If no format matches, `NA` is returned with a warning.

## See also

[read_results](https://osu-bee-lab.github.io/buzzr/reference/read_results.md)
which calls this internally when `posix_formats` is supplied.

## Examples

``` r
# Single file, single format (YYMMDD_HHMM)
file_start_time(
  'soybean/9/230809_0000_buzzdetect.csv',
  posix_formats = '%y%m%d_%H%M',
  tz = 'America/New_York'
)
#> [1] "2023-08-09 EDT"

# Multiple files in one call
paths <- c(
  'soybean/9/230809_0000_buzzdetect.csv',
  'chicory/1_104/250704_0000_buzzdetect.csv'
)
file_start_time(paths, posix_formats = '%y%m%d_%H%M', tz = 'America/New_York')
#> [1] "2023-08-09 EDT" "2025-07-04 EDT"

# Two formats for two recorder types in the same experiment;
# first_match = FALSE returns NA (with a warning) if both match a file
file_start_time(
  paths,
  posix_formats = c('%y%m%d_%H%M', '%Y%m%d_%H%M%S'),
  tz = 'America/New_York',
  first_match = FALSE
)
#> [1] "2023-08-09 EDT" "2025-07-04 EDT"
```
