#' @export
frames_expected <- function(binwidth_min, framelength_s){
  (binwidth_min*60)/framelength_s
}
