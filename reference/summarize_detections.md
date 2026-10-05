# Summarize detections and frames, optionally by group.

Sums all `detections_` columns and the `frames` column, either across
the entire data frame or within groups. Detection rates are then
(re)calculated from these summed totals, so any pre-existing
`detectionrate_` columns are dropped rather than averaged.

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
  and `results` has no groups, the entire data frame is summarized into
  a single row.

- calculate_rate:

  If `TRUE`, adds a `detectionrate_` column for each `detections_`
  column, calculated as detections divided by frames. Values range from
  0 to 1.

## Value

A data frame with one row per group (or one row total), the summed
`detections_` columns, a summed `frames` column, and optionally
`detectionrate_` columns.

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

# Summarize the entire data frame
summarize_detections(results)
#> Warning: No groups given or detected for results; summarizing entire data frame
#>    detections_ins_buzz frames detectionrate_ins_buzz
#>                  <num>  <num>                  <num>
#> 1:                   6     25                   0.24
```
