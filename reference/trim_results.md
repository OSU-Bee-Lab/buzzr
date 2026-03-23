# Trim activation columns in a results data frame.

Rounds all `activation_` columns to the specified number of decimal
places. Optionally retains only the neurons named in `neurons_keep`,
dropping all others. This is useful for reducing file size before
archiving or sharing results. The original data frame is not modified.

## Usage

``` r
trim_results(results, activation_digits, neurons_keep = NULL)
```

## Arguments

- results:

  A data frame or data.table with `activation_` columns.

- activation_digits:

  Integer. Number of decimal places to round activation values to.

- neurons_keep:

  Character vector of neuron names to retain. Names may include or omit
  the `activation_` prefix (e.g. `"ins_buzz"` and
  `"activation_ins_buzz"` are equivalent). All other activation columns
  are dropped. If `NULL` (default), all neurons are kept.

## Value

A data.table with rounded (and optionally filtered) activation columns.

## See also

[trim_directory](https://osu-bee-lab.github.io/buzzr/reference/trim_directory.md)
to apply this to an entire folder of files.

## Examples

``` r
path <- system.file(
  'extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv',
  package = 'buzzr'
)
results <- read_results(path)

# Round all activation columns to 2 decimal places
trim_results(results, activation_digits = 2)
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

# Keep only the ins_buzz neuron and round to 1 decimal place
trim_results(results, activation_digits = 1, neurons_keep = 'ins_buzz')
#>        start_filetime activation_ins_buzz
#>                 <num>               <num>
#>     1:           0.00                -1.2
#>     2:           0.96                -2.0
#>     3:           1.92                -2.1
#>     4:           2.88                -2.0
#>     5:           3.84                -2.0
#>    ---                                   
#> 89996:       86395.20                -2.7
#> 89997:       86396.16                -2.4
#> 89998:       86397.12                -2.9
#> 89999:       86398.08                -2.4
#> 90000:       86399.04                -3.1
```
