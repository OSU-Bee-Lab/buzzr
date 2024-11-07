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

  dt_sum <- dt[, bs_meanrate(detectionrate), by = groupby]
  return(dt_sum)
}


#' Calculate quantiles of detection rates
#'
#' @param dt A data.table with a detectionrate column
#' @param groupby A vector of column names by which the quantiles should be grouped
#' @param conf The desired confidence interval(s) to calculate quantiles; e.g., 0.95 computes 2.5th and 97.5th percentiles
#' @return A data.table
#' @export
quantile_rate <- function(dt, groupby, conf=0.50, median=T){
  quants <- sapply(
    conf,
    function(cnf){
      x <- (1-cnf)/2
      q <- c(x, 1-x)
      return(q)
    }
  )
  if(median){quants <- c(quants, 0.5)}

  dt_summ <- dt[
    ,
    {
      q_vals <- quantile(detectionrate, probs=quants, na.rm=T)
      names(q_vals) <- sub('\\%', '', names(q_vals))

      setNames(as.list(q_vals), paste0("quant_", names(q_vals)))
    }
    ,
    by = groupby
  ]

  return(dt_summ)
}

