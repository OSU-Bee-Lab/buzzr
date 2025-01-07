#' Non-parametric bootstrapping of detection rates
#'
#' @param dt A data.table with a detectionrate column
#' @param groupby A vector of column names by which the bootstraps should be grouped
#' @param replicates The number of replicates to draw for bootstrapping; passed to the R argument of one.boot
#' @param conf The desired confidence interval
#' @return A data.table
#'
#' @importFrom data.table :=
#' @importFrom data.table setDT
#' @importFrom data.table data.table
#'
#' @export
bootstrap_rate <- function(dt, groupby, boot_func=median, replicates=1e4, conf=0.95){
  dt = data.table::setDT(dt)

  translate_ci_row <- function(ci_row){
    conf_remainder <- 1 - ci_row[1]
    low <- paste0('conf_', conf_remainder/2)
    hi <- paste0('conf_', 1-(conf_remainder/2))

    vals <- ci_row[4:5]
    names(vals) <-  c(low, hi)

    return(vals)
  }

  translate_ci <- function(ci){
    if(is.null(ci)){return(NULL)}
    per <- ci$percent
    out <- unlist(apply(per, 1, translate_ci_row, simplify=F))
    return(out)
  }

  bs_meanrate <- function(rates){
    boot <- simpleboot::one.boot(rates, boot_func, replicates)
    ci <- boot::boot.ci(boot, conf=conf, type='perc')

    out <- c(center = boot_func(rates))
    out <- c(out, translate_ci(ci))
    out <- as.list(out)
    return(out)
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
quantile_rate <- function(dt, groupby, conf=0.50, take_median=T, take_mean=F){
  quants <- sapply(
    conf,
    function(cnf){
      x <- (1-cnf)/2
      q <- c(x, 1-x)
      return(q)
    }
  )
  if(take_median){quants <- c(quants, 0.5)}

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

  # TODO: can this be rolled into summary conditionally?
  if(take_mean){
    dt_mean <- dt[
      ,
      list(quant_mean = mean(detectionrate)),
      by = groupby
    ]

    dt_summ <- dt_mean[dt_summ, on=groupby]
  }

  return(dt_summ)
}

