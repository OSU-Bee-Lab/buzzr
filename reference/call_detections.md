# Call event detections using activation thresholds.

Converts raw neuron activation values into binary detections by applying
a numeric threshold to each named neuron. Frames where the activation
exceeds the threshold are marked `TRUE`; all others are `FALSE`. All
`activation_` columns are dropped from the output — use
[bin](https://osu-bee-lab.github.io/buzzr/reference/bin.md) afterwards
to summarise detection counts over time.

## Usage

``` r
call_detections(results, thresholds)
```

## Arguments

- results:

  A data frame or data.table of buzzdetect results, such as created by
  [read_results](https://osu-bee-lab.github.io/buzzr/reference/read_results.md)..
  Must contain `activation_` columns for each neuron named in
  `thresholds`.

- thresholds:

  A named numeric vector mapping neuron names to detection thresholds
  (e.g. `c(ins_buzz = -1.2)`). Frames whose activation value *exceeds*
  the threshold are counted as detections. Because `model_general_v3`
  outputs negative log-likelihoods, thresholds are typically negative.
  Activations *above* the threshold value are counted as detections, so
  thresholds for models that output negative log-likelihoods (such as
  `model_general_v3`) are typically negative (e.g. `-1.2`).

## Value

A data.table with `detections_` columns replacing the `activation_`
columns. Each detection column contains logical (`TRUE`/`FALSE`) values.

## Details

The original data frame is never modified; a copy is returned.

## See also

[bin](https://osu-bee-lab.github.io/buzzr/reference/bin.md) to count
detections per time bin,
[bin_directory](https://osu-bee-lab.github.io/buzzr/reference/bin_directory.md)
to run the full pipeline in one call.

## Examples

``` r
path <- system.file(
  'extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv',
  package = 'buzzr'
)
results <- read_results(path)

# Single neuron: activations above -1.2 are counted as buzz detections
call_detections(results, thresholds = c(ins_buzz = -1.2))
#>        start_filetime detections_ins_buzz
#>                 <num>              <lgcl>
#>     1:           0.00               FALSE
#>     2:           0.96               FALSE
#>     3:           1.92               FALSE
#>     4:           2.88               FALSE
#>     5:           3.84               FALSE
#>    ---                                   
#> 89996:       86395.20               FALSE
#> 89997:       86396.16               FALSE
#> 89998:       86397.12               FALSE
#> 89999:       86398.08               FALSE
#> 90000:       86399.04               FALSE

# Multiple neurons at once
call_detections(results, thresholds = c(ins_buzz = -1.2, mech_plane = -2.0))
#>        start_filetime detections_ins_buzz detections_mech_plane
#>                 <num>              <lgcl>                <lgcl>
#>     1:           0.00               FALSE                  TRUE
#>     2:           0.96               FALSE                 FALSE
#>     3:           1.92               FALSE                 FALSE
#>     4:           2.88               FALSE                 FALSE
#>     5:           3.84               FALSE                 FALSE
#>    ---                                                         
#> 89996:       86395.20               FALSE                 FALSE
#> 89997:       86396.16               FALSE                 FALSE
#> 89998:       86397.12               FALSE                 FALSE
#> 89999:       86398.08               FALSE                 FALSE
#> 90000:       86399.04               FALSE                 FALSE
```
