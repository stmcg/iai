#' Simulated data set
#'
#' This data set was simulated to reflect a setting with
#' missingness-not-at-random and an incomplete auxiliary variable.
#'
#' @docType data
#'
#' @format A data frame that contains 9,297 rows and the following columns:
#' \describe{
#'   \item{\code{Y}}{A continuous outcome variable.}
#'   \item{\code{X1}}{A binary predictor variable.}
#'   \item{\code{X2}}{A binary predictor variable.}
#'   \item{\code{W}}{A binary auxiliary variable.}
#' }
#'
#' @details
#' \strong{Variable dependencies:} The underlying values of the variables were generated as follows:
#' \itemize{
#'   \item \code{X1} is generated independently.
#'   \item \code{X2} depends on \code{X1}.
#'   \item \code{W} depends on \code{X2}.
#'   \item \code{Y} depends on \code{X1}, \code{X2}, and their interaction.
#' }
#'
#' \strong{Missingness patterns:} The missingness patterns were generated as follows:
#' \itemize{
#'   \item Missingness in \code{X1}, \code{X2}, and \code{Y} depends on the
#'     underlying (potentially unobserved) values of \code{W}.
#'   \item Missingness in \code{W} is generated independently.
#'   \item Rows where all variables are missing are removed from the dataset.
#' }
#'
#' @seealso \code{\link{mia_nice}}, \code{\link{mia_ice}}
#'
#' @keywords datasets
"dat.sim"
