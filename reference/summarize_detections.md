# Summarize detections and frames, optionally by group.

Sums all `detections_` columns and the `frames` column within groups.
Detection rates are then (re)calculated from these summed totals, so any
pre-existing `detectionrate_` columns are dropped rather than averaged.

## Usage

``` r
summarize_detections(results, groupcols = NULL, calculate_rate = T)
```

## Arguments

- results:

  A data frame or data.table of buzzdetect results, such as created by
  [read_results](https://osu-bee-lab.github.io/buzzr/reference/read_results.md).

- groupcols:

  Character vector of column names to group by before summarizing. If
  `NULL` (default) and `results` is already a grouped data frame (e.g.
  via dplyr::group_by), its existing groups are used instead. If `NULL`
  and `results` has no groups, no summarizing is done and detection
  rates are calculated for each row.

- calculate_rate:

  If `TRUE`, adds a `detectionrate_` column for each `detections_`
  column, calculated as detections divided by frames. Values range from
  0 to 1. If there are no groups and this is `FALSE`, `results` is
  returned unmodified with a warning.

## Value

With groups: a data frame with one row per group, the summed
`detections_` columns, a summed `frames` column, and optionally
`detectionrate_` columns. Without groups: `results` with the same rows,
plus (re)calculated `detectionrate_` columns if `calculate_rate` is
`TRUE`.

## Details

If no groups are given or detected, rows are left as-is and
`detectionrate_` columns are calculated in place.

## See also

[bin](https://osu-bee-lab.github.io/buzzr/reference/bin.md) to summarize
frame-level results into time bins, which is usually done before further
summarizing with this function.

## Examples

``` r
results <- data.frame(
  site                = c('a', 'a', 'b'),
  detections_ins_buzz = c(1, 3, 2),
  frames              = c(10, 10, 5)
)

# Summarize by group
summarize_detections(results, groupcols = 'site')
#>      site detections_ins_buzz frames detectionrate_ins_buzz
#>    <char>               <num>  <num>                  <num>
#> 1:      a                   4     20                    0.2
#> 2:      b                   2      5                    0.4

# No groups: add detection rates to each row
summarize_detections(results)
#>   site detections_ins_buzz frames detectionrate_ins_buzz
#> 1    a                   1     10                    0.1
#> 2    a                   3     10                    0.3
#> 3    b                   2      5                    0.4
```
