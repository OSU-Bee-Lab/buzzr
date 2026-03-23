#' Calculate the expected number of frames in a time bin.
#'
#' A convenience function for checking whether a binned results data.table has
#' the right number of frames. Frames per bin = `(binwidth_min * 60) / framelength_s`.
#' Compare against the `frames` column of a binned result to identify bins with
#' missing audio (e.g. recorder dropouts or files shorter than the bin width).
#'
#' @param binwidth_min Numeric. Bin width in minutes.
#' @param framelength_s Numeric. Frame length in seconds.
#'   For `model_general_v3` this is `0.96`.
#' @return A single numeric value: the expected number of frames per full bin.
#' @examples
#' # model_general_v3 uses 0.96-second frames
#' frames_expected(binwidth_min = 5,  framelength_s = 0.96)  # 312.5
#' frames_expected(binwidth_min = 20, framelength_s = 0.96)  # 1250
#' frames_expected(binwidth_min = 60, framelength_s = 0.96)  # 3750
#'
#' # Check for bins with incomplete audio coverage
#' \dontrun{
#' binned <- bin_directory(dir, thresholds = c(ins_buzz = -1.2), binwidth = 20)
#' expected <- frames_expected(20, 0.96)
#' binned[frames < expected * 0.9]  # bins with > 10% missing audio
#' }
#' @export
frames_expected <- function(binwidth_min, framelength_s){
  (binwidth_min*60)/framelength_s
}
