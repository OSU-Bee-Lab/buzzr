# Summarize a buzzdetect log file

Evaluate performance in a buzzdetect analysis log. Reports:

- duration_total - how many seconds did the analysis take?

- duration_audio - how many seconds of audio were analyzed?

- analysis_rate - how many seconds of audio were analyzed per second of
  wall time? NOTE! This does NOT use duration_total as the divisor, it
  starts counting when the first chunk is reported.

- bottleneck_time - how long did analyzers wait for streamers to fill
  the queue?

- duration_warmup - how long from the launching of analysis until the
  first chunk was analyzed? Includes results checking and cleaning,
  streamer launching, etc.

## Usage

``` r
evaluate_log(path_log)
```

## Arguments

- path_log:

  String. The path to a buzzdetect results log.

## Value

A named vector of analysis metrics

## Details

Useful for tuning analysis settings.
