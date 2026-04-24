# Read all buzzdetect result files in a directory (recursively).

Applies
[read_results](https://osu-bee-lab.github.io/buzzr/reference/read_results.md)
to every buzzdetect result file found recursively under `dir_results`
and row-binds the results into a single data.table. All arguments are
forwarded to
[read_results](https://osu-bee-lab.github.io/buzzr/reference/read_results.md).

## Usage

``` r
read_directory(
  dir_results,
  posix_formats = NA,
  first_match = FALSE,
  drop_filetime = TRUE,
  dir_nesting = NULL,
  return_ident = FALSE,
  tz = NA,
  workers = 2
)
```

## Arguments

- dir_results:

  The directory holding all buzzdetect results to be read.

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

- workers:

  Number of parallel workers to use when processing results. Set to
  `Inf` to use all available cores. Note that because data.table already
  uses multiple threads, you may want to set fewer workers than there
  are cores on your machine. Overridden by MC_CORES environmental
  variable if set.

## Value

A data.table combining all files, with the same columns as
[read_results](https://osu-bee-lab.github.io/buzzr/reference/read_results.md)
plus an optional `ident` column.

## Details

Returns an empty `data.frame` (with a warning) if no result files are
found, which allows safe use in pipelines with
[data.table::rbindlist](https://rdrr.io/pkg/data.table/man/rbindlist.html)
or similar.

## See also

[bin_directory](https://osu-bee-lab.github.io/buzzr/reference/bin_directory.md)
to also apply thresholds and bin in one call.

## Examples

``` r
dir <- system.file('extdata/five_flowers', package = 'buzzr')

# Read all five files and combine into one data.table
read_directory(
  dir,
  posix_formats = '%y%m%d_%H%M',
  tz            = 'America/New_York',
  dir_nesting   = c('flower', 'recorder')
)
#>             flower recorder      start_datetime activation_ins_buzz
#>             <char>   <char>              <POSc>               <num>
#>      1:    chicory    1_104 2025-07-04 00:00:00                -1.7
#>      2:    chicory    1_104 2025-07-04 00:00:00                -2.0
#>      3:    chicory    1_104 2025-07-04 00:00:01                -2.1
#>      4:    chicory    1_104 2025-07-04 00:00:02                -2.0
#>      5:    chicory    1_104 2025-07-04 00:00:03                -2.0
#>     ---                                                            
#> 449996: watermelon     1_73 2024-07-27 23:59:55                -1.7
#> 449997: watermelon     1_73 2024-07-27 23:59:56                -1.6
#> 449998: watermelon     1_73 2024-07-27 23:59:57                -1.7
#> 449999: watermelon     1_73 2024-07-27 23:59:58                -1.3
#> 450000: watermelon     1_73 2024-07-27 23:59:59                -1.6
#>         activation_ambient_rain activation_ins_trill activation_mech_plane
#>                           <num>                <num>                 <num>
#>      1:                    -2.0                 -1.9                  -2.2
#>      2:                    -2.0                 -1.9                  -2.4
#>      3:                    -2.8                 -2.8                  -3.4
#>      4:                    -2.6                 -2.4                  -3.2
#>      5:                    -2.3                 -2.4                  -2.5
#>     ---                                                                   
#> 449996:                    -1.8                 -1.9                  -2.0
#> 449997:                    -2.2                 -2.3                  -2.5
#> 449998:                    -2.1                 -2.1                  -2.4
#> 449999:                    -1.9                 -2.0                  -1.8
#> 450000:                    -2.2                 -2.1                  -2.2

# Also include the ident column for tracing results back to their source file
read_directory(dir, return_ident = TRUE)
#>                               ident start_filetime activation_ins_buzz
#>                              <char>          <num>               <num>
#>      1:   chicory/1_104/250704_0000           0.00                -1.7
#>      2:   chicory/1_104/250704_0000           0.96                -2.0
#>      3:   chicory/1_104/250704_0000           1.92                -2.1
#>      4:   chicory/1_104/250704_0000           2.88                -2.0
#>      5:   chicory/1_104/250704_0000           3.84                -2.0
#>     ---                                                               
#> 449996: watermelon/1_73/240727_0000       86395.20                -1.7
#> 449997: watermelon/1_73/240727_0000       86396.16                -1.6
#> 449998: watermelon/1_73/240727_0000       86397.12                -1.7
#> 449999: watermelon/1_73/240727_0000       86398.08                -1.3
#> 450000: watermelon/1_73/240727_0000       86399.04                -1.6
#>         activation_ambient_rain activation_ins_trill activation_mech_plane
#>                           <num>                <num>                 <num>
#>      1:                    -2.0                 -1.9                  -2.2
#>      2:                    -2.0                 -1.9                  -2.4
#>      3:                    -2.8                 -2.8                  -3.4
#>      4:                    -2.6                 -2.4                  -3.2
#>      5:                    -2.3                 -2.4                  -2.5
#>     ---                                                                   
#> 449996:                    -1.8                 -1.9                  -2.0
#> 449997:                    -2.2                 -2.3                  -2.5
#> 449998:                    -2.1                 -2.1                  -2.4
#> 449999:                    -1.9                 -2.0                  -1.8
#> 450000:                    -2.2                 -2.1                  -2.2
```
