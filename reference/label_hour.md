# A simplified scale_x_datetime label that returns only the hour of the day (for use with [commontime](https://osu-bee-lab.github.io/buzzr/reference/commontime.md))

A simplified scale_x_datetime label that returns only the hour of the
day (for use with
[commontime](https://osu-bee-lab.github.io/buzzr/reference/commontime.md))

## Usage

``` r
label_hour()
```

## Value

A *function* that takes POSIX values and returns only their hours. See
examples for use.

## Examples

``` r
# ggplot2::scale_x_datetime(labels=buzzr::label_hour())
```
