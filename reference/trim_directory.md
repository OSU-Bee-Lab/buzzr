# Trim activation columns for all result files in a directory.

Applies
[trim_results](https://osu-bee-lab.github.io/buzzr/reference/trim_results.md)
to every buzzdetect result file found recursively in `dir_results`,
saving each trimmed file in `dir_trim` while preserving the original
directory structure.

## Usage

``` r
trim_directory(
  dir_results,
  dir_trim,
  activation_digits,
  neurons_keep = NULL,
  output_format = "rds",
  if_exists = "stop",
  workers = 1
)
```

## Arguments

- dir_results:

  Path to the directory containing buzzdetect result files.

- dir_trim:

  Path to the output directory. Created automatically if it does not
  exist.

- activation_digits:

  Integer. Number of decimal places to round activation values to.

- neurons_keep:

  Character vector of neuron names to retain (see
  [trim_results](https://osu-bee-lab.github.io/buzzr/reference/trim_results.md)).
  If `NULL` (default), all neurons are kept.

- output_format:

  Output file format. One of `"rds"` (default) or `"csv"`. CSV files are
  written without row names.

- if_exists:

  What to do if an output file already exists. One of `"stop"` (default,
  throws an error), `"skip"` (silently skips existing files), or
  `"overwrite"` (overwrites with a warning).

- workers:

  Number of parallel workers. Defaults to `1` (sequential). Parallelism
  uses [parallel::mcmapply](https://rdrr.io/r/parallel/mclapply.html)
  and may not be supported on all platforms.

## Value

Invisibly returns a character vector of output file paths.

## See also

[trim_results](https://osu-bee-lab.github.io/buzzr/reference/trim_results.md)
for the single-file version.

## Examples

``` r
if (FALSE) { # \dontrun{
dir_in  <- system.file('extdata/five_flowers', package = 'buzzr')
dir_out <- file.path(tempdir(), 'five_flowers_trimmed')

# Trim all files, rounding to 2 decimal places, saved as .rds
trim_directory(dir_in, dir_out, activation_digits = 2)

# Save trimmed files as CSV instead
trim_directory(dir_in, dir_out, activation_digits = 2, output_format = 'csv')

# Re-run, keeping only the ins_buzz neuron and overwriting existing files
trim_directory(
  dir_in, dir_out,
  activation_digits = 2,
  neurons_keep = 'ins_buzz',
  if_exists = 'overwrite'
)
} # }
```
