#' Aggregate for results of NBP_classify
#'
#' This function shows the use of aggregate for dataframes that result from
#' the function NBP_classify. You can either use this wrapper, or use aggregate
#' or dplyr or other.
#'
#' @param p_val A vector of p-values
#' @param by A dataframe of columns to aggregate by, similar to the use in the
#' aggregate function.
#' @param type Type of aggregation.
#' @param evidence.limit See documentation of NBP_aggregate_fischer
#'
#' @return
#' @export
#'
#' @examples
#' NBP_aggregate_p_values(c(1E-3, 1E-3, 1E-3, 1E-3, 1), by=data.frame(by1=c(1,2,3,2,3)))
#'
#'
NBP_aggregate_p_values = function(p_val, by, type="fischer", evidence.limit=6.33E-5){
  ## Wrapper for NBP_aggregate_fischer
  ## You do not strictly need this function, it just calls aggregate
  ## Using Base R for package stability and CRAN

  ## by: Same as in aggregate function
  stopifnot(is.data.frame(by))

  ## If it is a matrix or a simple vector:
  p_val = as.data.frame(p_val)
  stopifnot(nrow(by)==nrow(p_val))

  if (type=="fischer") {
    agg = aggregate(p_val, by = by, FUN=NBP_aggregate_fischer, evidence.limit=evidence.limit)
    attr(agg, "evidence.limit") = evidence.limit
    return(agg)
  }

  stop(paste("The type", type, "is not implemented"))
}
