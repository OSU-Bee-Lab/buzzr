# Convert a date-time to a time of day as proportion (0,1) or hour (0,24; with decimals).

Useful for statistical analyses, e.g. modeling the time of peak
foraging. For plotting, use
[commontime](https://osu-bee-lab.github.io/buzzr/reference/commontime.md)
and
[label_hour](https://osu-bee-lab.github.io/buzzr/reference/label_hour.md).

## Usage

``` r
time_of_day(times, time_format = "proportion")
```

## Arguments

- times:

  A vector of POSIX times.
