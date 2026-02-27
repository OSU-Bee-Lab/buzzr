# Bin results by time and count detections

Given a results file, group results into bins by time and sum any
detection\_ columns. Can re-bin previously binned results, though this
gives weird results if the new bin is not a multiple of the old (e.g.,
bin(10), then bin(15)). Optionally, calculate the detection rate for
detection\_ columns (simply the total detections divided by the total
frames). Generally, calculating detection rate is best as a final step.
For example, you may first bin into 1 minute bins to compress the
dataset, but then re-bin by 15 minutes for graphing, re-bin by 1 hour
for one model and, re-bin by 1 day for another.

## Usage

``` r
bin(results, binwidth, calculate_rate = F)
```

## Arguments

- binwidth:

  The desired width of the bin in minutes

- calculate_rate:

  Should the detection rate for detection\_ columns be calculated? Takes
  total detections, divides by the number of frames, and outputs as a
  detectionrate\_ column.

- thresholds:

  A named numeric vector with names corresponding to neuron names and
  values corresponding to the desired detection threshold for that
  neuron.

## Value

A data.table with a bin\_ time column (bin_filetime or bin_datetime),
the same detection\_ columns as the input, and a frames column counting
the total frames in the bin.

## Details

Drops all activation columns.
