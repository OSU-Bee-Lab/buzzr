#' buzzr: Tools for wrangling buzzdetect results
#'
#' buzzr provides a pipeline for turning raw [buzzdetect](https://github.com/OSU-Bee-Lab/buzzdetect)
#' results into plotable, modelable, useful results.
#'
#' @section Do it the easy way:
#'
#' Run [buzzr::bin_directory] to automate all of the steps below.
#'
#' @section Step-by-step workflow:
#'
#' A step-by-step workflow can be nice if you're still working out your pipeline.
#' For example, maybe you want to tune your threshold, so you don't want [buzzr::bin_directory] to
#' re-read all of the results every time. First [buzzr::read_directory],
#' then try out a bunch of different thresholds to [buzzr::call_detections].
#'
#' **1. Read results**
#'
#' Point [buzzr::read_directory] at the output folder from buzzdetect.
#' Optionally, let buzzr convert file times to real-world date times
#' by supplying a POSIX format string (e.g. `'%y%m%d_%H%M'`) and time zone.
#' Tell buzzr how to understand your data organization by turning path
#' levels (e.g. ./year/site/recorder/foo_buzzdetect.csv) into columns.
#'
#' **2. Call detections**
#'
#' Pass the raw activation values through [buzzr::call_detections] with a named
#' threshold vector (e.g. `c(ins_buzz = -1.2)`).
#' This produces a `detections_` column for each named neuron, where the
#' value is `TRUE` if its activation exceeds your threshold for that frame.
#'
#' **3. Bin by time**
#'
#' Bin frame-level detections into fixed-width time bins with [buzzr::bin]
#' Set `calculate_rate = TRUE` to add a
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
#'   \item{[buzzr::bin_directory]}{One-stop-shop. Read, threshold, and bin an entire results folder in one call.}
#'   \item{[buzzr::read_results]}{Read a single buzzdetect CSV or RDS result file.}
#'   \item{[buzzr::call_detections]}{Apply activation thresholds to produce binary detections.}
#'   \item{[buzzr::bin]}{Aggregate frame-level results into time bins.}
#'   \item{[buzzr::commontime]}{Collapse dates onto a shared time-of-day axis for plotting.}
#'   \item{[buzzr::theme_buzzr]}{A ggplot2 theme styled for buzzdetect result plots.}
#'
#' }
#'
#' @section Useful tools:
#' \describe{
#'   \item{[buzzr::trim_directory]}{Dataset too big? Drop unnecessary detail and save as compressed RDS objects (faster to read, too!)}
#' }
#'
#' @keywords internal
"_PACKAGE"
