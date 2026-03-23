# Extract the identifier (ident) from a buzzdetect result file path.

Strips the results directory prefix, the `_buzzdetect` tag, and the file
extension, leaving a clean relative path that uniquely identifies the
recording. This ident can be used to locate corresponding audio files,
annotation files, or metadata.

## Usage

``` r
get_ident(path_in, dir_in = "")
```

## Arguments

- path_in:

  Character vector of file paths.

- dir_in:

  The root results directory to strip from the front of each path.
  Defaults to `''` (returns the full path minus extension and tag).

## Value

A character vector of the same length as `path_in`.

## Details

For example, given `dir_in = '/data/results'` and
`path_in = '/data/results/soybean/9/230809_0000_buzzdetect.csv'`, the
returned ident is `'soybean/9/230809_0000'`.

## See also

[read_directory](https://osu-bee-lab.github.io/buzzr/reference/read_directory.md)
and
[bin_directory](https://osu-bee-lab.github.io/buzzr/reference/bin_directory.md)
which can add the ident as a column via `return_ident = TRUE`.

## Examples

``` r
dir  <- system.file('extdata/five_flowers', package = 'buzzr')
path <- system.file(
  'extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv',
  package = 'buzzr'
)

# Returns 'soybean/9/230809_0000'
get_ident(path, dir)
#> [1] "soybean/9/230809_0000"

# Works on a vector of paths
all_paths <- list.files(dir, pattern = '_buzzdetect', recursive = TRUE,
                        full.names = TRUE)
get_ident(all_paths, dir)
#> [1] "chicory/1_104/250704_0000"   "mustard/54/240904_0000"     
#> [3] "pumpkin/1_37/240808_0000"    "soybean/9/230809_0000"      
#> [5] "watermelon/1_73/240727_0000"
```
