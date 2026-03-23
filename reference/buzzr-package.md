# buzzr: Tools for wrangling buzzdetect results

buzzr provides a pipeline for turning raw
[buzzdetect](https://github.com/OSU-Bee-Lab/buzzdetect) results into
plotable, modelable, useful results.

## Do it the easy way

Run
[bin_directory](https://osu-bee-lab.github.io/buzzr/reference/bin_directory.md)
to automate all of the steps below.

## Step-by-step workflow

A step-by-step workflow can be nice if you're still working out your
pipeline. For example, maybe you want to tune your threshold, so you
don't want
[bin_directory](https://osu-bee-lab.github.io/buzzr/reference/bin_directory.md)
to re-read all of the results every time. First
[read_directory](https://osu-bee-lab.github.io/buzzr/reference/read_directory.md),
then try out a bunch of different thresholds to
[call_detections](https://osu-bee-lab.github.io/buzzr/reference/call_detections.md).

**1. Read results**

Point
[read_directory](https://osu-bee-lab.github.io/buzzr/reference/read_directory.md)
at the output folder from buzzdetect. Optionally, let buzzr convert file
times to real-world date times by supplying a POSIX format string (e.g.
`'%y%m%d_%H%M'`) and time zone. Tell buzzr how to understand your data
organization by turning path levels (e.g.
./year/site/recorder/foo_buzzdetect.csv) into columns.

**2. Call detections**

Pass the raw activation values through
[call_detections](https://osu-bee-lab.github.io/buzzr/reference/call_detections.md)
with a named threshold vector (e.g. `c(ins_buzz = -1.2)`). This produces
a `detections_` column for each named neuron, where the value is `TRUE`
if its activation exceeds your threshold for that frame.

**3. Bin by time**

Bin frame-level detections into fixed-width time bins with
[bin](https://osu-bee-lab.github.io/buzzr/reference/bin.md) Set
`calculate_rate = TRUE` to add a `detectionrate_` column (detections /
frames).

**4. Plot**

Use
[commontime](https://osu-bee-lab.github.io/buzzr/reference/commontime.md)
to collapse recordings from different dates onto a shared time-of-day
axis,
[label_hour](https://osu-bee-lab.github.io/buzzr/reference/label_hour.md)
to format the x-axis, and
[theme_buzzr](https://osu-bee-lab.github.io/buzzr/reference/theme_buzzr.md)
for a clean ggplot2 theme.

For a complete worked example see
[`vignette('buzzr')`](https://osu-bee-lab.github.io/buzzr/articles/buzzr.md).

## Key functions

- [bin_directory](https://osu-bee-lab.github.io/buzzr/reference/bin_directory.md):

  One-stop-shop. Read, threshold, and bin an entire results folder in
  one call.

- [read_results](https://osu-bee-lab.github.io/buzzr/reference/read_results.md):

  Read a single buzzdetect CSV or RDS result file.

- [call_detections](https://osu-bee-lab.github.io/buzzr/reference/call_detections.md):

  Apply activation thresholds to produce binary detections.

- [bin](https://osu-bee-lab.github.io/buzzr/reference/bin.md):

  Aggregate frame-level results into time bins.

- [commontime](https://osu-bee-lab.github.io/buzzr/reference/commontime.md):

  Collapse dates onto a shared time-of-day axis for plotting.

- [theme_buzzr](https://osu-bee-lab.github.io/buzzr/reference/theme_buzzr.md):

  A ggplot2 theme styled for buzzdetect result plots.

## Useful tools

- [trim_directory](https://osu-bee-lab.github.io/buzzr/reference/trim_directory.md):

  Dataset too big? Drop unnecessary detail and save as compressed RDS
  objects (faster to read, too!)

## See also

Useful links:

- <https://OSU-Bee-Lab.github.io/buzzr>

- <https://github.com/OSU-Bee-Lab/buzzdetect>
