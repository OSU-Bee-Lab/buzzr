# Coerce all dates to the same day, preserving time of day

For all input times, the date will be coerced to 2000-01-01 and the time
zone of choice (defaults to your current time zone). This makes for
prettier graphing than using time of day as a numeric (e.g., allows
ggplot2::scale_x_datetime) and allows plotting data from different days
on a common X axis.

## Usage

``` r
commontime(times, tz = Sys.timezone())
```

## Arguments

- times:

  A vector of POSIX times.
