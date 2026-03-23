# Convert a date-time to a numeric time of day.

Returns the time of day as either a proportion of the 24-hour day (`0` =
midnight, `0.5` = noon, `1` = next midnight) or as a decimal hour (`0` =
midnight, `6.5` = 6:30 AM, `23.5` = 11:30 PM). Seconds are included in
the calculation.

## Usage

``` r
time_of_day(times, time_format = "proportion")
```

## Arguments

- times:

  A POSIXct vector.

- time_format:

  One of `'proportion'` (default) or `'hour'`.

## Value

A numeric vector of the same length as `times`.

## Details

This is useful for statistical analyses — for example, fitting a
circular model of daily activity or finding the hour of peak buzz
detections. For plotting, use
[commontime](https://osu-bee-lab.github.io/buzzr/reference/commontime.md)
and
[label_hour](https://osu-bee-lab.github.io/buzzr/reference/label_hour.md)
instead.

## See also

[commontime](https://osu-bee-lab.github.io/buzzr/reference/commontime.md)
for a POSIXct alternative suitable for plotting.

## Examples

``` r
times <- as.POSIXct(
  c('2023-08-09 00:00:00', '2023-08-09 06:00:00', '2023-08-09 18:30:00'),
  tz = 'America/New_York'
)

# Midnight = 0, 6 AM = 0.25, noon = 0.5
time_of_day(times)
#> [1] 0.0000000 0.2500000 0.7708333

# As decimal hours: midnight = 0, 6 AM = 6, 6:30 PM = 18.5
time_of_day(times, time_format = 'hour')
#> [1]  0.0  6.0 18.5
```
