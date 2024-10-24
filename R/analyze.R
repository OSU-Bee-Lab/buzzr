#' Non-parametric bootstrapping of detection rates
#'
#' @param dt A data.table with a detectionrate column
#' @param groupby A vector of column names by which the bootstraps should be grouped
#' @param replicates The number of replicates to draw for bootstrapping; passed to the R argument of one.boot
#' @param conf The desired confidence interval
#' @return A data.table
#' @export
bootstrap_rate <- function(dt, groupby, replicates=1e4, conf=0.95){
  bs_meanrate <- function(rates){
    ci <- simpleboot::one.boot(rates, mean, replicates) %>%
      boot::boot.ci(type='perc')

    return(list(rate_mean = mean(rates), rate_low=ci$percent[,4], rate_high=ci$percent[5]))
  }

  dt_sum <- data[, bs_meanrate(detectionrate), by = groupby]
  return(dt_sum)
}
