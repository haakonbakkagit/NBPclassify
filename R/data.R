#' Fish scale element composition data
#'
#' Fish from 4 locations and escaped fish were gathered and
#' the element composition of their scales were analysed with mass spectrometry.
#' The logarithmic values of the element concentrations are given in this
#' dataset.
#'
#' There are two variants of the dataset: FVF and SVF. These are Norwegian
#' acronyms for fresh water phase and for sea water phase. This means that
#' the samples are taken in two different ways from the fish scales, leading
#' to two different methods by which we can try to classify the escaped fish.
#' The variables Location, ID_fish and Id_scale refer to the same physical
#' fish scale for the two datasets. Hence, it is possible to merge the datasets
#' if you want to.
#'
#' The mass spectrometry technique used has a minimum detection level, which is
#' c(0.26, 1.8, 1.1, 0.03, 1.0, 590, 0.28, 9.7, 0.56)
#' for
#' Li7, B11, Ba137, U238, Mg24, S32, Mn55, Xn66, and Sr88 respectively.
#' When the measurement is below this minimum, the value in the dataset
#' is the logarithm of half the minimum detection value.
#'
#' \describe{
#'   \item{Location}{One of four locations (A,D,M,T), or NA for the escaped
#'   fish}
#'   \item{Id_fish}{The ID of each fish}
#'   \item{Id_scale}{The ID of each fish scale within a fish}
#'   \item{Is_escape}{Logical, whether the fish was escaped.
#'   Equivalent to is.na(Location)}
#'   \item{Li7, B11, ...}{Remaining columns are names after each element,
#'   and contain log values of the concentration of the element found in each
#'   fish scale.}
#' }
#' @source Data extracted and anonymised from a project funded by
#' a private-public partnership between Sporbarhet AS and the Norwegian Veterinary
#' Institute.
#' @name Fish_scale_elements_fvf
#' @examples
#' data("Fish_scale_elements_fvf")
#' data("Fish_scale_elements_svf")
#'

#' @rdname Fish_scale_elements_fvf
"Fish_scale_elements_fvf"

#' @rdname Fish_scale_elements_fvf
"Fish_scale_elements_svf"
