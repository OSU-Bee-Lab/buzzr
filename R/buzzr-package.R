#' buzzr: Tools for wrangling buzzdetect results
#'
#' buzzr provides a pipeline for reading, thresholding, and summarising output
#' files from [buzzdetect](https://github.com/OSU-Bee-Lab/buzzdetect), a deep
#' learning tool for detecting insect buzz-pollination in passive acoustic
#' recordings.
#'
#' @section Typical workflow:
#'
#' **1. Read results**
#'
#' Point [buzzr::read_results] or [buzzr::read_directory] at your buzzdetect
#' output. Supply a POSIX format string (e.g. `'%y%m%d_%H%M'`) to parse
#' real-world date-times from file names, and `dir_nesting` to turn directory
#' levels (e.g. site, recorder) into columns.
#'
#' **2. Call detections**
#'
#' Pass the raw activation values through [buzzr::call_detections] with a named
#' threshold vector (e.g. `c(ins_buzz = -1.2)`). Frames whose activation exceeds
#' the threshold become `TRUE` in a `detections_` column.
#'
#' **3. Bin by time**
#'
#' Summarise frame-level detections into fixed-width time bins with [buzzr::bin]
#' or [buzzr::bin_directory]. Set `calculate_rate = TRUE` to add a
#' `detectionrate_` column (detections / frames).
#'
#' **4. Plot**
#'
#' Use [buzzr::commontime] to collapse recordings from different dates onto a
#' shared time-of-day axis, [buzzr::label_hour] to format the x-axis, and
#' [buzzr::theme_buzzr] for a clean ggplot2 theme.
#'
#' For a complete worked example see `vignette('buzzr')`.
#'
#' @section Key functions:
#' \describe{
#'   \item{[buzzr::bin_directory]}{Read, threshold, and bin an entire results folder in one call.}
#'   \item{[buzzr::read_results]}{Read a single buzzdetect CSV or RDS result file.}
#'   \item{[buzzr::call_detections]}{Apply activation thresholds to produce binary detections.}
#'   \item{[buzzr::bin]}{Aggregate frame-level results into time bins.}
#'   \item{[buzzr::commontime]}{Collapse dates onto a shared time-of-day axis for plotting.}
#'   \item{[buzzr::theme_buzzr]}{A ggplot2 theme styled for buzzdetect result plots.}
#' }
#'
#' @keywords internal
"_PACKAGE"
