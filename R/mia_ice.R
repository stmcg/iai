#' MIA Method via the Iterative Conditional Expectation Approach
#'
#' This function implements the marginalization over incomplete auxiliaries (MIA) method (Mathur et al. 2026) via an iterative conditional expectation (ICE) approach. For an outcome variable \eqn{Y}, predictor variable \eqn{X}, and auxiliary variable \eqn{W}, this function estimates the conditional outcome mean identified by
#' \deqn{
#' \mu_{\text{MIA}}(x) = \int_{w} E [ Y | X=x, W=w, M=1 ] p( w | X=x, R_W = R_X = 1 ) dw,
#' }
#' where \eqn{R_W} and \eqn{R_X} are indicators of non-missing values of \eqn{W} and \eqn{X}, respectively, and \eqn{M} is an indicator of a complete case pattern (i.e., \eqn{Y}, \eqn{X}, and \eqn{W} are non-missing).
#' This function uses a plug-in estimator of the functional represented as
#' \deqn{
#' \mu_{\text{MIA}}(x) = E [ \, E [ Y | X, W, M=1 ] \mid X=x, R_W = R_X = 1 \, ]
#' }
#' The function supports estimating the identifying functionals of \eqn{\mu_{\text{MIA}}(x_1)} and \eqn{\mu_{\text{MIA}}(x_2)} as well as contrasts between them (differences, ratios).
#'
#' @param data Data frame containing the observed data.
#' @param X_names Vector of character strings specifying the name(s) of the predictor variable(s) \eqn{X}.
#' @param X_values_1 Numeric vector specifying the value of the predictor variable(s) \eqn{X}, i.e. \eqn{x_1} in \eqn{\mu_{\text{MIA}}(x_1)}.
#' @param X_values_2 (Optional) Numeric vector specifying an additional value of the predictor variable(s) \eqn{X}, i.e. \eqn{x_2} in \eqn{\mu_{\text{MIA}}(x_2)}.
#' @param contrast_type (Optional) Character string specifying the type of contrast to use when comparing \eqn{\mu_{\text{MIA}}(x_1)} and \eqn{\mu_{\text{MIA}}(x_2)}. Options are \code{"difference"}, \code{"ratio"}, and \code{"none"}.
#' @param Y_model Formula for the outcome model.
#' @param outer_model Formula for the outer regression of the fitted outcome means on the predictor(s) \eqn{X}. The left-hand side must be \code{g_hat}, which denotes the fitted outcome means \eqn{\hat{E}[ Y | X, W, M = 1 ]}. For example, \code{g_hat ~ X1 * X2} specifies a model that is saturated with respect to binary predictors \code{X1} and \code{X2}.
#' @param Y_type (Optional) Character string specifying the "type" of the outcome variable. Options are \code{"binary"} and \code{"continuous"}. If this is not supplied, the type will be inferred from the corresponding column in \code{data}.
#'
#' @return An object of class "mia". This object is a list with the following elements:
#' \item{mean_est_1}{conditional outcome mean estimate under \code{X_values_1}}
#' \item{mean_est_2}{conditional outcome mean estimate under \code{X_values_2}}
#' \item{contrast_est}{contrast of conditional outcome mean estimates between \code{X_values_1} and \code{X_values_2}}
#' \item{fit_Y}{fitted model for Y}
#' \item{fit_outer}{fitted outer regression of the fitted outcome means on \eqn{X}}
#' \item{...}{additional elements}
#' @seealso \code{\link{print.mia}}, \code{\link{get_CI}}
#'
#' @details
#'
#' \strong{Estimation algorithm:}
#'
#' \emph{Step 1:} One fits a model for the conditional outcome mean \eqn{g(x, w) = E [ Y | X=x, W=w, M=1 ]} using the complete cases (i.e., units with \eqn{Y}, \eqn{X}, and \eqn{W} observed). Let \eqn{\hat{g}(x, w)} denote the fitted model.
#'
#' \emph{Step 2:} For every unit with \eqn{X} and \eqn{W} observed (i.e., \eqn{R_X = R_W = 1}), one computes the fitted outcome mean \eqn{\hat{g}(X, W)} at the unit's observed \eqn{X} and \eqn{W}.
#'
#' \emph{Step 3:} One regresses the fitted outcome means \eqn{\hat{g}(X, W)} on the predictor(s) \eqn{X} among units with \eqn{R_X = R_W = 1}, using the model specified by \code{outer_model}, and takes the prediction at \eqn{X = x} as the estimate of \eqn{\mu_{\text{MIA}}(x)}. Note that when \code{outer_model} is saturated with respect to \eqn{X} (e.g., \code{g_hat ~ X1 * X2} for binary predictors), this reduces to the sample mean of \eqn{\hat{g}(x, w)} among units with \eqn{X = x} and \eqn{R_X = R_W = 1}.
#'
#'
#' @references
#' Mathur MB, Seaman S, Zhang W, McGrath S, Shpitser I. (2026). \emph{Estimating conditional means under missingness-not-at-random with incomplete auxiliary variables}. \doi{10.13140/RG.2.2.30750.19524}.
#' @examples
#' set.seed(1234)
#' mia_ice(data = dat.sim,
#'         X_names = c("X1", "X2"),
#'         X_values_1 = c(0, 1), X_values_2 = c(0, 0),
#'         Y_model = Y ~ W + X1 + X2, outer_model = g_hat ~ X1 * X2)
#'
#'
#' @export

mia_ice <- function(data, X_names, X_values_1, X_values_2 = NULL,
                    contrast_type,
                    Y_model, Y_type,
                    outer_model) {

  # Checking that data has the correct column names
  missing_cols <- setdiff(X_names, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("The following columns are listed in X_names but missing from the data:",
               paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  if (!'Y' %in% colnames(data)){
    stop("The observed data must include a column called 'Y' indicating the outcome variable.", call. = FALSE)
  }

  # Checking the model formula for Y
  if (!inherits(Y_model, "formula")) {
    stop("Y_model must be a formula, e.g., Y ~ W + X", call. = FALSE)
  }
  lhs_Y <- all.vars(Y_model[[2]])
  if (length(lhs_Y) != 1 || lhs_Y != "Y") {
    stop("The left-hand side of Y_model must be the variable 'Y'.", call. = FALSE)
  }

  # Checking the model formula for the outer regression
  if (missing(outer_model)){
    stop("outer_model must be supplied, e.g., g_hat ~ X1 * X2", call. = FALSE)
  }
  if (!inherits(outer_model, "formula")) {
    stop("outer_model must be a formula, e.g., g_hat ~ X1 * X2", call. = FALSE)
  }
  lhs_outer <- all.vars(outer_model[[2]])
  if (length(lhs_outer) != 1 || lhs_outer != "g_hat") {
    stop("The left-hand side of outer_model must be 'g_hat', which denotes the fitted outcome means.", call. = FALSE)
  }
  rhs_outer <- all.vars(outer_model[[3]])
  bad_outer <- setdiff(rhs_outer, X_names)
  if (length(bad_outer) > 0){
    stop(paste0("The right-hand side of outer_model can only depend on the predictor(s) in X_names. The following term(s) are not in X_names: ",
                paste(bad_outer, collapse = ", "), "."), call. = FALSE)
  }

  # Checking X_values_1 and X_values_2
  if (length(X_names) != length(X_values_1)){
    stop("The arguments 'X_names' and 'X_values_1' must be of the same length.", call. = FALSE)
  }
  if (!is.null(X_values_2)){
    if (length(X_names) != length(X_values_2)){
      stop("The arguments 'X_names' and 'X_values_2' must be of the same length.", call. = FALSE)
    }
    if (missing(contrast_type)){
      contrast_type <- 'difference'
    }
  } else {
    contrast_type <- 'none'
  }

  # Validating X_values for categorical predictors
  for (i in 1:length(X_names)){
    if (is.factor(data[[X_names[i]]])){
      valid_levels <- levels(data[[X_names[i]]])
      # Check X_values_1
      if (!X_values_1[i] %in% valid_levels){
        stop(paste0("The value ", X_values_1[i], " specified for predictor '", X_names[i],
                    "' in X_values_1 is not a valid level. Valid levels are: ",
                    paste(valid_levels, collapse = ", "), "."), call. = FALSE)
      }
      # Check X_values_2 if provided
      if (!is.null(X_values_2)){
        if (!X_values_2[i] %in% valid_levels){
          stop(paste0("The value ", X_values_2[i], " specified for predictor '", X_names[i],
                      "' in X_values_2 is not a valid level. Valid levels are: ",
                      paste(valid_levels, collapse = ", "), "."), call. = FALSE)
        }
      }
    }
  }

  # Checking variable types are appropriately set
  if (!missing(Y_type)){
    if (!Y_type %in% c('binary', 'continuous')){
      stop("Y_type must be set to either 'binary' or 'continuous'.", call. = FALSE)
    }
  }

  # Identifying the auxiliary variable(s) in Y_model (i.e., the predictors of the
  # outcome model that are not in X_names). These must be observed for a unit to
  # contribute to the outer step.
  W_names <- setdiff(all.vars(Y_model[[3]]), X_names)

  # Creating datasets for fitting models
  R_X <- stats::complete.cases(data[, X_names])
  R_W <- stats::complete.cases(data[, W_names])
  R_Y <- !is.na(data$Y)
  data_fit_Y <- data[R_X == 1 & R_W == 1 & R_Y == 1, ]
  data_outer <- data[R_X == 1 & R_W == 1, ]

  # Checking for empty datasets after filtering
  if (nrow(data_fit_Y) == 0){
    stop("No complete cases found for fitting the outcome model (Y). All observations are missing at least one of: Y, X, or W. Please check the missingness patterns in your data.", call. = FALSE)
  }
  if (nrow(data_outer) == 0){
    stop("No cases found with X and W observed for fitting the outer regression. All observations are missing at least one of: X or W. Please check the missingness patterns in your data.", call. = FALSE)
  }

  # Fitting Y model
  if (missing(Y_type)){
    Y_levels <- unique(stats::na.omit(data$Y))
    if (length(Y_levels) == 2){
      Y_type <- 'binary'
    } else if (is.numeric(data$Y)){
      Y_type <- 'continuous'
    } else {
      Y_type <- NA
    }
    # Check if type inference failed
    if (is.na(Y_type)){
      stop("Unable to infer the type for the outcome variable Y. Please explicitly specify Y_type. Valid options are 'binary' or 'continuous'.", call. = FALSE)
    }
  }
  fit_Y <- safe_fit(variable_name = 'Y', variable_type = Y_type,
                    formula = Y_model, data = data_fit_Y)

  # Computing the fitted outcome means at the observed X and W
  data_outer$g_hat <- get_Y_pred(df = data_outer, fit_Y = fit_Y, Y_type = Y_type)

  # Outer regression (a single model serves all target predictor values)
  fit_outer <- stats::lm(outer_model, data = data_outer)

  # Prediction (for X_values_1)
  Y_mean <- get_ice_mean(X_values = X_values_1, X_names = X_names,
                         fit_outer = fit_outer)

  # Prediction (for X_values_2)
  if (!is.null(X_values_2)){
    Y_mean_2 <- get_ice_mean(X_values = X_values_2, X_names = X_names,
                             fit_outer = fit_outer)
    if (contrast_type == 'difference'){
      contrast_est <- Y_mean - Y_mean_2
    } else if (contrast_type == 'ratio'){
      contrast_est <- Y_mean / Y_mean_2
    } else if (contrast_type == 'none'){
      contrast_est <- NA
    }
  } else {
    Y_mean_2 <- contrast_est <- NA
  }

  out <- list(
    mean_est_1 = Y_mean,
    mean_est_2 = Y_mean_2,
    contrast_est = contrast_est,
    fit_Y = fit_Y,
    fit_outer = fit_outer,
    Y_type = Y_type,
    X_names = X_names,
    W_names = W_names,
    X_values_1 = X_values_1, X_values_2 = X_values_2,
    contrast_type = contrast_type,
    Y_model = Y_model,
    outer_model = outer_model,
    method = 'ice',
    data = data
  )
  class(out) <- 'mia'
  return(out)
}

get_ice_mean <- function(X_values, X_names, fit_outer){
  # Prediction from the outer regression at the target X value.
  df_pred <- data.frame(matrix(X_values, nrow = 1))
  colnames(df_pred) <- X_names
  mean_est <- as.numeric(stats::predict(fit_outer, newdata = df_pred))

  return(mean_est)
}

get_Y_pred <- function(df, fit_Y, Y_type){
  if (Y_type == 'binary'){
    Y_pred <- stats::predict(fit_Y, type = 'response', newdata = df)
  } else if (Y_type == 'continuous'){
    Y_pred <- stats::predict(fit_Y, newdata = df)
  }
  return(Y_pred)
}
