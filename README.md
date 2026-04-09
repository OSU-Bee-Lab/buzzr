# buzzr

buzzr is an R companion package to [buzzdetect](https://github.com/OSU-Bee-Lab/buzzdetect), a passive acoustic monitoring tool for pollinators. It provides a tidy pipeline for reading, thresholding, binning, and plotting buzzdetect results.

See [the walkthrough vignette](https://osu-bee-lab.github.io/buzzr/articles/buzzr.html) for a full introduction.

## Installation
buzzr is not yet on CRAN. Install via `devtools`:


```r
install.packages('devtools')
devtools::install_github('OSU-Bee-Lab/buzzr')
```

## Key features

**One-shot.** `bin_directory()` combines most preprocessing operations into a single function call. Point it at your results, pull the trigger, and you're halfway to plotting and modeling.

**Structure is data.** If your results are grouped into folders by site, treatment, etc., that metadata can be interpreted from the file structure. The `dir_nesting` argument decodes folder structure into columns. 

**Names are data, too!** `file_start_time()` extracts recording start times from file names using POSIX format strings

**Honey, I shrunk the data.** `trim_directory()` rounds activation values and drops unwanted neurons across an entire results folder. The output folder can be less than 4% the size of the raw buzzdetect results.

**Vroom vroom.** Squeeze every last drop of performance out of buzzdetect using `evaluate_log()` to calculate your analysis rate and identify bottlenecks.

