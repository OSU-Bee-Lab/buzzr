# Trim activation columns for all result files in a directory.

Applies
[trim_results](https://osu-bee-lab.github.io/buzzr/reference/trim_results.md)
to every buzzdetect result file found recursively in `dir_results`,
saving the trimmed output to `path_out`.

## Usage

``` r
trim_directory(
  dir_results,
  path_out,
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

- path_out:

  Path to the output file (`.rds` or `.csv`) or output directory.
  Directories are created automatically if they do not exist. When a
  file path is given, its extension overrides `output_format`.

- activation_digits:

  Integer. Number of decimal places to round activation values to.

- neurons_keep:

  Character vector of neuron names to retain (see
  [trim_results](https://osu-bee-lab.github.io/buzzr/reference/trim_results.md)).
  If `NULL` (default), all neurons are kept.

- output_format:

  Output file format when `path_out` is a directory. One of `"rds"`
  (default) or `"csv"`. Ignored when `path_out` is a file path.

- if_exists:

  What to do if an output file already exists. One of `"stop"` (default,
  throws an error), `"skip"` (silently skips existing files), or
  `"overwrite"` (overwrites with a warning).

- workers:

  Number of parallel workers. Defaults to `1` (sequential). Parallelism
  uses [parallel::mcmapply](https://rdrr.io/r/parallel/mclapply.html)
  and may not be supported on all platforms. Ignored when `path_out` is
  a single file.

## Value

Invisibly returns the output file path(s).

## Details

If `path_out` ends in `.rds` or `.csv`, all results are read via
[read_directory](https://osu-bee-lab.github.io/buzzr/reference/read_directory.md)
(with `return_ident = TRUE`), trimmed, and written to that single file
(overriding `output_format`). Otherwise, `path_out` is treated as an
output directory and files are written there preserving the original
directory structure.

## See also

[trim_results](https://osu-bee-lab.github.io/buzzr/reference/trim_results.md)
for the single-file version.

## Examples

``` r
if (FALSE) { # \dontrun{
dir_in  <- system.file('extdata/five_flowers', package = 'buzzr')
dir_out <- file.path(tempdir(), 'five_flowers_trimmed')

# Trim all files, rounding to 2 decimal places, saved as .rds in a directory
trim_directory(dir_in, dir_out, activation_digits = 2)

# Combine all results into a single .rds file
trim_directory(dir_in, file.path(tempdir(), 'trimmed.rds'), activation_digits = 2)

# Combine all results into a single CSV
trim_directory(dir_in, file.path(tempdir(), 'trimmed.csv'), activation_digits = 2)

# Re-run, keeping only the ins_buzz neuron and overwriting existing files
trim_directory(
  dir_in, dir_out,
  activation_digits = 2,
  neurons_keep = 'ins_buzz',
  if_exists = 'overwrite'
)
} # }
```
