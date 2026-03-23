# ggplot2 x-axis label formatter for time-of-day plots.

Returns a labeller function that formats POSIXct values as readable hour
strings (e.g. `"6 am"`, `"2 pm"`). Designed for use with
ggplot2::scale_x_datetime after converting your time column with
[commontime](https://osu-bee-lab.github.io/buzzr/reference/commontime.md).

## Usage

``` r
label_hour(tz = Sys.timezone())
```

## Arguments

- tz:

  Time zone string (e.g. `'America/New_York'`). Defaults to your system
  time zone. Should match the `tz` argument passed to
  [commontime](https://osu-bee-lab.github.io/buzzr/reference/commontime.md).

## Value

A function that accepts a POSIXct vector and returns a character vector
of hour labels.

## Details

**Time zone tip:** use the same `tz` here as you passed to
[commontime](https://osu-bee-lab.github.io/buzzr/reference/commontime.md)
so that the axis labels match your data.

## See also

[commontime](https://osu-bee-lab.github.io/buzzr/reference/commontime.md)
to prepare your time column,
[theme_buzzr](https://osu-bee-lab.github.io/buzzr/reference/theme_buzzr.md)
for a matching plot theme.

## Examples

``` r
# Typical usage with ggplot2 (not run):
# ggplot(df, aes(x = commontime(bin_datetime, tz = 'America/New_York'),
#               y = detectionrate_ins_buzz, color = flower)) +
#   geom_line() +
#   scale_x_datetime(labels = label_hour(tz = 'America/New_York')) +
#   theme_buzzr()

# The labeller itself:
fmt <- label_hour(tz = 'America/New_York')
fmt(as.POSIXct('2000-01-01 08:00:00', tz = 'America/New_York'))  # "8 am"
#> [1] "8 am"
fmt(as.POSIXct('2000-01-01 14:00:00', tz = 'America/New_York'))  # "2 pm"
#> [1] "2 pm"
```
