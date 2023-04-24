#' Classification with NBP
#'
#' NBP is Naive Bayes with p-values/exceedance probabilities, unlike the standard Naive
#' Bayes which uses relative probability densities. However, this algorithm
#' can also be used to perform Naive Bayes.
#' See also Arxiv paper (TODO).
#'
#' @param train_cov Covariates for the training data
#' @param train_class Class membership (response category) for the training data.
#' Must be the same length as number of rows in train_cov.
#' @param predict_cov Covariates for the prediction, where the class is not known.
#' @param keep Do you want the density functions for plotting?
#' @param family The distribution family for the covariates.
#' Only Gaussian is implemented.
#' @param identifiers Set of identifiers or metadata, belonging to the prediction covariates.
#' Will be added to the output dataframe.
#'
#' @return A dataframe with p-values for predicting using the predict_cov at
#' each of the different possible classes.
#' @export
#'
#' @examples
#' NBP_classify(iris[-c(1,2), 1:4], iris[-c(1,2), 5], iris[c(1,2), 1:4])
#'
#'
NBP_classify = function(train_cov, train_class, predict_cov, keep=FALSE,
                        family="Gaussian", identifiers=NULL) {
  stopifnot(family=="Gaussian") # only family implemented yet

  ## Check dimensions
  train_class = drop(train_class)
  stopifnot(nrow(train_cov) == length(train_class))
  if (!is.null(identifiers)) {
    stopifnot(nrow(identifiers)==nrow(predict_cov))
  }
  if(any(names(predict_cov)=="nbp_class")) stop("The name nbp_class is reserved")
  if(any(names(predict_cov)=="row_names")) stop("The name row_names is reserved")
  stopifnot(is.data.frame(train_cov))
  stopifnot(is.data.frame(predict_cov))
  stopifnot(ncol(train_cov)==ncol(predict_cov))
  stopifnot(all(names(train_cov)==names(predict_cov)))

  ## Create identifiers
  if (is.null(identifiers)) {
    if (!is.null(rownames(predict_cov))) {
      identifiers = data.frame(row_names = rownames(predict_cov))
    }
  }

  ## Compute general variables
  classes = as.character(unique(train_class))
  nclass = length(classes)
  ncov = ncol(train_cov)
  npred = nrow(predict_cov)

  ## Expand indetifiers
  identif_expand = identifiers[rep(1:nrow(identifiers), ncov), , drop=F]

  ## Setup empty variables
  p_vals = data.frame()
  ## Preserve row and column names:
  ptab = predict_cov*0
  ldens = ptab

  if (keep) flist = list()

  for (c in 1:nclass) {
    ## We will create nbp[[c]]
    if (keep) flist[[c]] = list()

    for (j in 1:ncov) {
      xtrain = train_cov[train_class==classes[c], j]

      ## Fit Gaussian
      mu = median(xtrain, na.rm=T)
      sigma = sd(xtrain, na.rm=T)

      ## Compute p value
      a = pnorm(predict_cov[, j], mean = mu, sd = sigma, log = F)
      pval = 1-2*abs(a-0.5)
      ptab[, j] = pval

      ## Compute log densities
      ldens[, j] = dnorm(predict_cov[, j], mean = mu, sd = sigma, log = T)

      ## If (keep==true): store a library of density functions for plotting.
      ## Later: Also store parameters?
      ## TODO: Test it
      ## TODO: Check that function environment contains only the minimal needs of the function! Delete the big dataframes
      if (keep) {
        flist[[c]][[j]] = function(x) {
          mu = mu
          sigma = sigma
          y = dnorm(x, mu, sigma)
        }
      }
    } # end for j

    ## Make a long format
    ptab2 = stack(ptab)
    ## Restructure and rename
    ptab2$ind = as.character(ptab2$ind)
    names(ptab2) = c("p_val", "covariate")
    ptab2 = ptab2[ , c(2,1)]

    ldens2 = stack(ldens)

    ## Add a column with the class
    ## Add identifiers
    ptab3 = data.frame(identif_expand, nbp_class=classes[c], ptab2,
                       logdens = ldens2[, 1])
    ##TODO: Delete rownames
    ## Add these predictions to the total result
    p_vals = rbind(p_vals, ptab3)
    # rownames(pvals) = NULL

  } # end for c

  # nbp = list(p_vals=p_vals)
  if (keep) attr(p_vals, "flist") = flist
  return(p_vals)

}
