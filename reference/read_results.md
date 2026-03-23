# Read a single buzzdetect results file.

Reads a buzzdetect result file at any stage of analysis using
[data.table::fread](https://rdrr.io/pkg/data.table/man/fread.html)
(`.csv`) or [base::readRDS](https://rdrr.io/r/base/readRDS.html)
(`.rds`). The raw buzzdetect `"start"` column is renamed to
`"start_filetime"` so that buzzr can tell apart file-relative timestamps
(seconds from the start of the audio file) from real-world date-times.

## Usage

``` r
read_results(
  path_results,
  posix_formats = NA,
  first_match = FALSE,
  drop_filetime = TRUE,
  tz = NA,
  dir_nesting = NULL
)
```

## Arguments

- path_results:

  The path to any buzzdetect result file that can be read with
  read_results.

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

- tz:

  Time zone string passed to
  [base::as.POSIXct](https://rdrr.io/r/base/as.POSIXlt.html) (e.g.
  `'America/New_York'`). See
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html) for valid
  values.. Ignored if the results already contain a `start_datetime` or
  `bin_datetime` column.

- dir_nesting:

  A character vector used to name the directory levels above the results
  file. Each element becomes a column in the output, storing the
  components of the path. For example, dir_nesting = c('site',
  'recorder') for the path
  data/2026/siteA/recorder_4/250704_11343_buzzdetect.csv would add a
  site column (holding the value 'siteA') and a recorder column (holding
  the value 'recorder_4').

## Value

A data.table with a `start_filetime` or `start_datetime` column, one
column per neuron activation or detection, and optionally one column per
level of `dir_nesting`.

## Details

Optionally, the recording's start date-time is parsed from the file name
and used to convert `start_filetime` into an absolute `start_datetime`
column. Directory levels above the file can also be extracted into their
own columns via `dir_nesting`, which is particularly useful when
combining many files with
[read_directory](https://osu-bee-lab.github.io/buzzr/reference/read_directory.md).

## See also

[read_directory](https://osu-bee-lab.github.io/buzzr/reference/read_directory.md)
to read all files in a folder at once,
[call_detections](https://osu-bee-lab.github.io/buzzr/reference/call_detections.md)
to apply detection thresholds,
[bin](https://osu-bee-lab.github.io/buzzr/reference/bin.md) to summarise
results into time bins.

## Examples

``` r
path <- system.file(
  'extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv',
  package = 'buzzr'
)

# Basic read — 'start' column is renamed to 'start_filetime'
read_results(path)
#>        start_filetime activation_ins_buzz activation_ambient_rain
#>                 <num>               <num>                   <num>
#>     1:           0.00                -1.2                    -1.3
#>     2:           0.96                -2.0                    -2.1
#>     3:           1.92                -2.1                    -2.8
#>     4:           2.88                -2.0                    -2.2
#>     5:           3.84                -2.0                    -2.3
#>    ---                                                           
#> 89996:       86395.20                -2.7                    -1.8
#> 89997:       86396.16                -2.4                    -1.5
#> 89998:       86397.12                -2.9                    -1.0
#> 89999:       86398.08                -2.4                    -0.6
#> 90000:       86399.04                -3.1                    -1.6
#>        activation_ins_trill activation_mech_plane
#>                       <num>                 <num>
#>     1:                 -1.7                  -1.7
#>     2:                 -2.7                  -2.6
#>     3:                 -3.0                  -2.9
#>     4:                 -2.6                  -2.5
#>     5:                 -2.8                  -2.6
#>    ---                                           
#> 89996:                 -2.9                  -3.1
#> 89997:                 -2.6                  -3.1
#> 89998:                 -2.9                  -3.5
#> 89999:                 -2.8                  -3.3
#> 90000:                 -3.0                  -3.1

# Parse real-world date-time from the filename (YYMMDD_HHMM format)
read_results(path, posix_formats = '%y%m%d_%H%M', tz = 'America/New_York')
#>             start_datetime activation_ins_buzz activation_ambient_rain
#>                     <POSc>               <num>                   <num>
#>     1: 2023-08-09 00:00:00                -1.2                    -1.3
#>     2: 2023-08-09 00:00:00                -2.0                    -2.1
#>     3: 2023-08-09 00:00:01                -2.1                    -2.8
#>     4: 2023-08-09 00:00:02                -2.0                    -2.2
#>     5: 2023-08-09 00:00:03                -2.0                    -2.3
#>    ---                                                                
#> 89996: 2023-08-09 23:59:55                -2.7                    -1.8
#> 89997: 2023-08-09 23:59:56                -2.4                    -1.5
#> 89998: 2023-08-09 23:59:57                -2.9                    -1.0
#> 89999: 2023-08-09 23:59:58                -2.4                    -0.6
#> 90000: 2023-08-09 23:59:59                -3.1                    -1.6
#>        activation_ins_trill activation_mech_plane
#>                       <num>                 <num>
#>     1:                 -1.7                  -1.7
#>     2:                 -2.7                  -2.6
#>     3:                 -3.0                  -2.9
#>     4:                 -2.6                  -2.5
#>     5:                 -2.8                  -2.6
#>    ---                                           
#> 89996:                 -2.9                  -3.1
#> 89997:                 -2.6                  -3.1
#> 89998:                 -2.9                  -3.5
#> 89999:                 -2.8                  -3.3
#> 90000:                 -3.0                  -3.1

# Keep both time columns and label directory levels
read_results(
  path,
  posix_formats  = '%y%m%d_%H%M',
  tz             = 'America/New_York',
  drop_filetime  = FALSE,
  dir_nesting    = c('flower', 'recorder')
)
#>         flower recorder      start_datetime start_filetime activation_ins_buzz
#>         <char>   <char>              <POSc>          <num>               <num>
#>     1: soybean        9 2023-08-09 00:00:00           0.00                -1.2
#>     2: soybean        9 2023-08-09 00:00:00           0.96                -2.0
#>     3: soybean        9 2023-08-09 00:00:01           1.92                -2.1
#>     4: soybean        9 2023-08-09 00:00:02           2.88                -2.0
#>     5: soybean        9 2023-08-09 00:00:03           3.84                -2.0
#>    ---                                                                        
#> 89996: soybean        9 2023-08-09 23:59:55       86395.20                -2.7
#> 89997: soybean        9 2023-08-09 23:59:56       86396.16                -2.4
#> 89998: soybean        9 2023-08-09 23:59:57       86397.12                -2.9
#> 89999: soybean        9 2023-08-09 23:59:58       86398.08                -2.4
#> 90000: soybean        9 2023-08-09 23:59:59       86399.04                -3.1
#>        activation_ambient_rain activation_ins_trill activation_mech_plane
#>                          <num>                <num>                 <num>
#>     1:                    -1.3                 -1.7                  -1.7
#>     2:                    -2.1                 -2.7                  -2.6
#>     3:                    -2.8                 -3.0                  -2.9
#>     4:                    -2.2                 -2.6                  -2.5
#>     5:                    -2.3                 -2.8                  -2.6
#>    ---                                                                   
#> 89996:                    -1.8                 -2.9                  -3.1
#> 89997:                    -1.5                 -2.6                  -3.1
#> 89998:                    -1.0                 -2.9                  -3.5
#> 89999:                    -0.6                 -2.8                  -3.3
#> 90000:                    -1.6                 -3.0                  -3.1
```
