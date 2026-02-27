# Call detections using thresholds

Call detections using thresholds

## Usage

``` r
call_detections(results, thresholds)
```

## Arguments

- results:

  A data frame of buzzdetect results, such as created by
  [read_results](https://osu-bee-lab.github.io/buzzr/reference/read_results.md)..
  Must have activation\_ columns corresponding to the neurons named in
  thresholds and must *not* have detections\_ column for the same.

- thresholds:

  A named numeric vector with names corresponding to neuron names and
  values corresponding to the desired detection threshold for that
  neuron.

## Examples

``` r
call_detections(
  results = buzzr::read_results('models/model_general_v3/output/testbuzz_buzzdetect.csv'),
  thresholds = c(ins_buzz = -1.2, ambient_rain = -1)
)
#> Error in data.table::fread(path_results): File 'models/model_general_v3/output/testbuzz_buzzdetect.csv' does not exist or is non-readable. getwd()=='/home/runner/work/buzzr/buzzr/docs/reference'
```
