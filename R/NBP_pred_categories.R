#' Turn p-values into interpretable categories
#'
#' This function implements simple logic on how to threshold continuous values
#' into discrete values.
#'
#' @param dat A dataframe with all numeric columns being p-values
#' @param type The type of categorisation you want. See code comments
#' inside the
#' function for details.
#' @param cutoff Some types require you to set a cutoff
#' @param do_checks Should extra checks be done if possible? Can result in
#' warnings that are helpful/unhelpful.
#'
#'
#'
#' @returns The original dataframe but where all numerical values have been
#' replaced by the thresholded values.
#'
#' @export
#'
#' @examples
#' NBP_pred_categories(c(0.1, 1E-10, 0, 1))
#'
NBP_pred_categories = function(dat, type="Avoid_0pred_V1", cutoff=0, do_checks=FALSE) {
  ## Want a dataframe with some numerical columns
  stopifnot(is.data.frame(dat))
  is_numeric <- unlist(lapply(dat, is.numeric), use.names = FALSE)

  table = dat[ , is_numeric]

  table = as.matrix(table)
  ## Return categories
  ## Preserve names
  cat = table*0

  ## Checks
  if (is.null(attr(table, "evidence.limit")) & do_checks) {
    do_checks = FALSE
    warning("No evidence.limit found, checks not performed.")
  }

  ## Check if integer columns
  ## Probably a mistake to use
  test = colSums(table%%1==0)
  if (any(test==nrow(table))) {
    warning("One of your columns is an integer column.
            Make sure that you want to threshold this one!
            If not, use as.character on it before calling this function.")
  }


  if (type == "Avoid_0pred_V1") {
    ## threshold
    if (do_checks){
      if (attr(table, "evidence.limit") != 6.33E-5) stop("
          This categorisation type was made for evidence limit 6.33E-5,
          which does not match your table")
    }

    #thres1 = 0.05
    ## A single datapoint can never say no
    ## Same as Grade 2 in Grading1to6
    thres1 = 6.32E-5
    cat = cat*0
    #cat[table<thres1] = 0
    cat[table>=thres1] = 1

  } else if (type=="simple_cutoff") {

    cat = cat*0
    cat[table>=cutoff] = 1

  } else if (type=="dens_max_minus_log6") {

    ## Here we assume log densities
    ## And just one column
    ## TODO: Make multicolumn!
    stopifnot(ncol(table)==1)
    cat = cat*0
    cat[table>(max(table)-log(6))] = 1

  } else if (type=="logp_max_minus_log6") {

    ## Here we assume p vals or densities (not log)
    ## Take log of entire table:
    table = log(table+1E-100)
    ## Afterwards: as above

    cat = cat*0
    for (i.row in 1:nrow(table)) {
      tmp = table[i.row, ]
      tmp = (tmp>(max(tmp)-log(6)))
      cat[i.row, tmp] = 1
    }


  } else if (type=="Grading1to6_V1") {

    cat = cat*0+1
    cat[table>8.1E-8] = 2
    ## Grade 2 is same as avoid_pred_0
    cat[table>6.32E-5] = 3
    cat[table>0.005] = 4
    cat[table>0.05] = 5
    cat[table>0.5] = 6
  } else if (type=="NEW") {
    stop("ADD")
  } else {
    stop("Not allowed: type")
  }

  ## Replace numeric columns in dat with the categories
  dat[ , is_numeric] = cat

  return(dat)
}
