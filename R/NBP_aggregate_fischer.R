#' Aggregation of p values with Fischer's method
#'
#' @param p_vals A vector of p-values
#' @param evidence.limit A threshold for the minimum p-value. This is used
#' before aggregation to ensure that no outlier p-value completely dominates
#' the overall aggregated p-value.
#' Up to 4 sd out for Gaussian: 1-2*abs(pnorm(4)-0.5) = 6.33e-05. 
#' Two of these aggregated with the function: 8.1E-8.
#' Up to 3.5 sd out for Gaussian: 1-2*abs(pnorm(3.5)-0.5) = 4.7E-4.
#' Two of these aggregated with the function: 3.6E-6.
#' 
#'
#' @return One p-value
#' @export
#'
#' @examples
#' NBP_aggregate_fischer(c(1E-3))
#' NBP_aggregate_fischer(c(1E-3, 1E-3))
#' NBP_aggregate_fischer(c(1E-3, 1E-3, 1E-3))
#' NBP_aggregate_fischer(c(1E-3, 1E-3, 1E-3, 1, 1))
#'
#'
NBP_aggregate_fischer = function(p_vals, evidence.limit=6.33E-5){
  ## Whatever the input may be, want a total aggregation

  if (is.list(p_vals)) p_vals = unlist(p_vals)
  # if (is.matrix(p_vals)) or character or integer or other:
  p_vals = as.numeric(p_vals)

  p_vals = p_vals[!is.na(p_vals)]

  if (any(p_vals[!is.na(p_vals)] < 0)) stop("Negative p values are illegal")
  if (any(p_vals[!is.na(p_vals)] > 1)) stop("P values above 1 are illegal")

  ## Limit the evidence contributed to
  ##    not be smaller than the evidence.limit
  ##    this to avoid outliers really killing us
  p_vals[p_vals < evidence.limit] = evidence.limit

  psum = -2*sum(log(p_vals), na.rm=T)
  ## Accounts for NA because they are removed:
  psum = 1-pchisq(psum, df=2*length(p_vals))

  if (is.na(psum)) {
    print(p_vals)
    print(psum)
    stop("Sum not valid")
  }

  return(psum)
}
