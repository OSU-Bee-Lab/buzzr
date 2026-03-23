# A ggplot2 theme for aesthetic plotting of buzzdetect results.

Applies a clean, publication-ready style with horizontal y-axis titles,
a border around each panel, and suppressed minor gridlines. Available in
light and dark variants — the dark variant uses the deep purple from
[palette](https://osu-bee-lab.github.io/buzzr/reference/palette.md) as
the plot background.

## Usage

``` r
theme_buzzr(base_size = 10, mode = "light")
```

## Arguments

- base_size:

  Numeric. Base font size in points. All text elements scale relative to
  this value. Defaults to `10`.

- mode:

  Character. Color scheme — `'light'` (default) for a white background,
  `'dark'` for a deep-purple background suited to presentations or
  spectrograms.

## Value

A [ggplot2::theme](https://ggplot2.tidyverse.org/reference/theme.html)
object that can be added to any ggplot.

## See also

[label_hour](https://osu-bee-lab.github.io/buzzr/reference/label_hour.md)
for a matching x-axis formatter,
[palette](https://osu-bee-lab.github.io/buzzr/reference/palette.md) for
the buzzdetect color palette,
[commontime](https://osu-bee-lab.github.io/buzzr/reference/commontime.md)
to prepare time-of-day x axes.

## Examples

``` r
# Light mode (default) — typical use in a detection-rate time-of-day plot:
# ggplot(binned, aes(x = commontime(bin_datetime, tz = 'America/New_York'),
#                    y = detectionrate_ins_buzz, color = flower)) +
#   geom_line() +
#   scale_x_datetime(labels = label_hour(tz = 'America/New_York')) +
#   labs(x = 'Time of day', y = 'Detection\nrate') +
#   theme_buzzr()

# Dark mode — useful for presentations or pairing with spectrograms:
# ggplot(binned, aes(x = commontime(bin_datetime, tz = 'America/New_York'),
#                    y = detectionrate_ins_buzz, color = flower)) +
#   geom_line() +
#   scale_x_datetime(labels = label_hour(tz = 'America/New_York')) +
#   theme_buzzr(base_size = 14, mode = 'dark')
```
