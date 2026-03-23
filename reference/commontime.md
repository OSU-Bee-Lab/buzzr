# Coerce all dates to the same day, preserving time of day.

Sets the date of every element in `times` to 2000-01-01 while keeping
the time-of-day unchanged. This lets you overlay recordings from
different calendar days on a shared x axis using
ggplot2::scale_x_datetime and
[label_hour](https://osu-bee-lab.github.io/buzzr/reference/label_hour.md).
The arbitrary date 2000-01-01 was chosen because it is far enough from
DST transition dates to avoid edge cases in most time zones.

## Usage

``` r
commontime(times, tz = Sys.timezone())
```

## Arguments

- times:

  A POSIXct vector.

- tz:

  Time zone string (e.g. `'America/New_York'`). Defaults to your system
  time zone. Should match the `tz` argument passed to
  [label_hour](https://osu-bee-lab.github.io/buzzr/reference/label_hour.md).

## Value

A POSIXct vector of the same length as `times`, with the date component
set to 2000-01-01.

## Details

**Time zone tip:** pass the same `tz` here as you pass to
[label_hour](https://osu-bee-lab.github.io/buzzr/reference/label_hour.md)
to ensure the hour labels match the data.

## See also

[label_hour](https://osu-bee-lab.github.io/buzzr/reference/label_hour.md)
for the matching x-axis label formatter,
[time_of_day](https://osu-bee-lab.github.io/buzzr/reference/time_of_day.md)
for a numeric alternative suitable for modelling.

## Examples

``` r
times <- as.POSIXct(
  c('2023-08-09 06:15:00', '2024-07-27 14:30:00'),
  tz = 'America/New_York'
)

# Both dates become 2000-01-01; times of day are preserved
commontime(times, tz = 'America/New_York')
#> [1] "2000-01-01 06:15:00 EST" "2000-01-01 14:30:00 EST"

# Typical ggplot2 usage (not run here):
# ggplot(df, aes(x = commontime(bin_datetime, tz = 'America/New_York'),
#               y = detectionrate_ins_buzz, color = flower)) +
#   geom_line() +
#   scale_x_datetime(labels = label_hour(tz = 'America/New_York'))
```
