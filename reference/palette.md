# The buzzdetect color palette.

A named discrete color palette derived from the magma color scale (which
is also commonly used for spectrograms). Colors range from deep purple
through fuchsia and salmon to a warm off-white.

## Usage

``` r
palette
```

## Format

An object of class `character` of length 8.

## Details

Named colors: `black`, `purple_deep`, `purple`, `periwinkle`, `fuchsia`,
`salmon`, `tangerine`, `white_hot`.

For a continuous magma palette see viridis::magma.

## Examples

``` r
# Access named colors directly
palette['fuchsia']
#>   fuchsia 
#> "#b8216c" 
palette[c('purple', 'salmon')]
#>    purple    salmon 
#> "#452390" "#f7504a" 
```
