#' MIA Method via the Noniterative Conditional Expectation (NICE) Approach
#'
#' This function implements the marginalization over incomplete auxiliaries (MIA) method (Mathur et al. 2026). For an outcome variable \eqn{Y}, predictor variable \eqn{X}, and auxiliary variable \eqn{W}, this function estimates the conditional outcome mean identified by
#' \deqn{
#' \mu_{\text{MIA}}(x) = \int_{w} E [ Y | X=x, W=w, M=1 ] p( w | X=x, R_W = R_X = 1 ) dw.
#' }
#' where \eqn{R_W} and \eqn{R_X} are indicators of non-missing values of \eqn{W} and \eqn{X}, respectively, and \eqn{M} is an indicator of a complete case pattern (i.e., \eqn{Y}, \eqn{X}, and \eqn{W} are non-missing).
#' The function supports estimating the identifying functionals of \eqn{\mu_{\text{MIA}}(x_1)} and \eqn{\mu_{\text{MIA}}(x_2)} as well as contrasts between them (differences, ratios).
#'
#' @param data Data frame containing the observed data.
#' @param X_names Vector of character strings specifying the name(s) of the predictor variable(s) \eqn{X}.
#' @param X_values_1 Numeric vector specifying the value of the predictor variable(s) \eqn{X}, i.e. \eqn{x_1} in \eqn{\mu_{\text{MIA}}(x_1)}.
#' @param X_values_2 (Optional) Numeric vector specifying an additional value of the predictor variable(s) \eqn{X}, i.e. \eqn{x_2} in \eqn{\mu_{\text{MIA}}(x_2)}.
#' @param contrast_type (Optional) Character string specifying the type of contrast to use when comparing \eqn{\mu_{\text{MIA}}(x_1)} and \eqn{\mu_{\text{MIA}}(x_2)}. Options are \code{"difference"}, \code{"ratio"}, and \code{"none"}.
#' @param Y_model Formula for the outcome model.
#' @param W_model Formula for the auxiliary variable model. If the auxiliary variable is multivariate, this argument should be a list of model formulas, one for each component. The components will be simulated in the order they appear in the list.
#' @param W_type  (Optional) Vector of character strings specifying the "type" of each auxiliary variable. Options are \code{"binary"}, \code{"categorical"}, and \code{"normal"}. If this is not supplied, the type will be inferred from the corresponding column in \code{data}.
#' @param Y_type (Optional) Character string specifying the "type" of the outcome variable. Options are \code{"binary"} and \code{"continuous"}. If this is not supplied, the type will be inferred from the corresponding column in \code{data}.
#' @param n_mc Integer specifying the number of Monte Carlo samples to use.
#' @param return_simulated_data Logical scalar indicating whether to return the simulated data set(s) containing the predictors and simulated auxiliary variable. Setting this argument to \code{TRUE} can substantially increase the size of the returned object, particularly when \code{n_mc} is large. The default is \code{FALSE}.
#'
#' @return An object of class "mia". This object is a list with the following elements:
#' \item{mean_est_1}{conditional outcome mean estimate under \code{X_values_1}}
#' \item{mean_est_2}{conditional outcome mean estimate under \code{X_values_2}}
#' \item{contrast_est}{contrast of conditional outcome mean estimates between \code{X_values_1} and \code{X_values_2}}
#' \item{fit_W}{a list of fitted model(s) for W}
#' \item{fit_Y}{fitted model for Y}
#' \item{simulated_data}{a list, where the first element is the simulated data set under \code{X_values_1} and the second element is the simulated data set under \code{X_values_2}. The simulated data sets contain the predictors and simulated auxiliary variable. This element is set to \code{NULL} unless \code{return_simulated_data} is set to \code{TRUE}.}
#' \item{...}{additional elements}
#' @seealso \code{\link{print.mia}}, \code{\link{get_CI}}
#'
#' @details
#'
#' \strong{Estimation algorithm:}
#'
#' \emph{Step 1:} One fits a model for the conditional outcome mean \eqn{E [ Y | X=x, W=w, M=1 ]} and the conditional density of the auxiliary variables \eqn{p( w | X=x, R_W = R_X = 1 )}. When \eqn{W} is multivariate, i.e., \eqn{W = (W_1, \dots, W_p)^\top}, one uses the decomposition
#' \deqn{p( w | X=x, R_W = R_X = 1 ) = \prod_{j = 1}^p p( w_j | X=x, w_1, \dots, w_{j-1}, R_W = R_X = 1 )}
#' and fits models for the components \eqn{p( w_j | X=x, w_1, \dots, w_{j-1}, R_W = R_X = 1 )}.
#'
#' \emph{Step 2:} Monte Carlo integration is used to compute the integral in the identifying functional for \eqn{\mu_{\text{MIA}}(x)} based on the fitted models in the first step. More specifically, for iteration \eqn{i}, the following algorithm is performed. The value of \eqn{W} is first simulated from its estimated conditional distribution. When \eqn{W} is multivariate, the components of \eqn{W} are simulated sequentially from their fitted models. That is, \eqn{W_1} is simulated conditional on \eqn{x}, \eqn{W_2} is simulated conditional on \eqn{x, W_1}, and so on. Then, the mean of \eqn{Y} is estimated conditional on \eqn{x, W}. Finally, the average of the estimated means (across all iterations \eqn{i}) is taken as the estimate of \eqn{\mu_{\text{MIA}}(x)}.
#'
#'
#' @references
#' Mathur MB, Seaman S, Zhang W, McGrath S, Shpitser I. (2026). \emph{Estimating conditional means under missingness-not-at-random with incomplete auxiliary variables}. \doi{10.13140/RG.2.2.30750.19524}.
#' @examples
#' set.seed(1234)
#' mia_nice(data = dat.sim,
#'          X_names = c("X1", "X2"),
#'          X_values_1 = c(0, 1), X_values_2 = c(0, 0),
#'          Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)
#'
#'
#' @export

mia_nice <- function(data, X_names, X_values_1, X_values_2 = NULL,
                contrast_type,
                Y_model, Y_type,
                W_model, W_type,
                n_mc = 10000,
                return_simulated_data = FALSE) {

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

  # Checking the names and model formulas for W
  if (!inherits(W_model, "formula") & !is.list(W_model)){
    stop("W_model must be a formula, e.g., W ~ X, or a list of formulas if W is multivariate", call. = FALSE)
  }
  if (inherits(W_model, "formula")){
    W_model <- list(W_model)
  }
  n_W <- length(W_model)
  W_names <- rep(NA, times = n_W)
  for (i in 1:n_W){
    W_model_temp <- W_model[[i]]
    if (!inherits(W_model_temp, "formula")) {
      stop(paste0("Element ", i, " in W_model is not a formula"), call. = FALSE)
    }
    lhs_W <- all.vars(W_model_temp[[2]])
    if (!lhs_W %in% colnames(data)){
      stop("The observed data must include a column called ", lhs_W, ", which was included as an auxiliary variable in W_model.", call. = FALSE)
    }
    W_names[i] <- lhs_W
  }
  for (i in 1:n_W){
    W_model_temp <- W_model[[i]]
    rhs_W <- all.vars(W_model_temp[[3]])
    if (any(W_names[i:n_W] %in% rhs_W)){
      bad_ind <- paste0(which(W_names[i:n_W] %in% rhs_W) + (i - 1), collapse = ', ')
      stop(paste0("Error in the specification of W_model. The model for component ", i, " (i.e., ", W_names[i], ") depends on the following component(s): ", bad_ind,
                  ". The model for each component can only depend on previous components, not subsequent ones."), call. = FALSE)
    }
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
  if (!missing(W_type)){
    for (i in 1:n_W){
      stop_message_beginning <- paste0("Error in the specification of element ", i, " in 'W_type.' ")
      if (!W_type[i] %in% c('binary', 'categorical', 'normal')){
        stop(paste0(stop_message_beginning, "W_type must be set to either 'binary', 'categorical', or 'normal'."), call. = FALSE)
      }
      if (W_type[i] == 'categorical'){
        if (length(unique(stats::na.omit(data[[W_names[i]]]))) == 2){
          stop(paste0(stop_message_beginning, "W_type should be set to 'binary' when W only has two levels."), call. = FALSE)
        } else if (!is.factor(data[[W_names[i]]])){
          stop(paste0(stop_message_beginning, "Auxiliary variables with type 'categorical' need to be encoded as factors in data."), call. = FALSE)
        }
      }
    }
  }
  if (!missing(Y_type)){
    if (!Y_type %in% c('binary', 'continuous')){
      stop("Y_type must be set to either 'binary' or 'continuous'.", call. = FALSE)
    }
  }

  # Creating datasets for fitting models
  R_X <- stats::complete.cases(data[, X_names])
  R_W <- stats::complete.cases(data[, W_names])
  R_Y <- !is.na(data$Y)
  data_fit_Y <- data[R_X == 1 & R_W == 1 & R_Y == 1, ]
  data_fit_W <- data[R_X == 1 & R_W == 1, ]

  # Checking for empty datasets after filtering
  if (nrow(data_fit_Y) == 0){
    stop("No complete cases found for fitting the outcome model (Y). All observations are missing at least one of: Y, X, or W. Please check the missingness patterns in your data.", call. = FALSE)
  }
  if (nrow(data_fit_W) == 0){
    stop("No complete cases found for fitting the auxiliary variable model (W). All observations are missing at least one of: X or W. Please check the missingness patterns in your data.", call. = FALSE)
  }

  # Fitting W model
  if (missing(W_type)){
    W_type <- rep(NA, times = n_W)
    for (i in 1:n_W){
      W_levels <- unique(stats::na.omit(data[[W_names[i]]]))
      if (length(W_levels) == 2){
        W_type[i] <- 'binary'
      } else if (is.factor(data[[W_names[i]]])){
        W_type[i] <- 'categorical'
      } else if (is.numeric(data[[W_names[i]]])){
        W_type[i] <- 'normal'
      }
    }
    # Check if type inference failed
    failed_inference <- which(is.na(W_type))
    if (length(failed_inference) > 0){
      failed_vars <- paste(W_names[failed_inference], collapse = ", ")
      stop(paste0("Unable to infer the type for auxiliary variable(s): ", failed_vars,
                 ". Please explicitly specify W_type for these variables. Valid options are 'binary', 'categorical', or 'normal'."), call. = FALSE)
    }
  }

  fit_W <- vector("list", length = n_W)
  for (i in 1:n_W){
    fit_W[[i]] <- safe_fit(variable_name = W_names[i], variable_type = W_type[i],
                           formula = W_model[[i]], data = data_fit_W)
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

  # Simulation of W and Y (for X_values_1)
  df_XW <- sim_W_all(X_values = X_values_1, X_names = X_names,
                     data_fit_W = data_fit_W, fit = fit_W,
                     type = W_type, n_mc = n_mc, W_names = W_names)
  Y_mean <- get_Y_mean(df = df_XW, fit_Y = fit_Y, Y_type = Y_type)

  # Simulation of W and Y (for X_values_2)
  if (!is.null(X_values_2)){
    df_XW_2 <- sim_W_all(X_values = X_values_2, X_names = X_names,
                         data_fit_W = data_fit_W, fit = fit_W,
                       type = W_type, n_mc = n_mc, W_names = W_names)
    Y_mean_2 <- get_Y_mean(df = df_XW_2, fit_Y = fit_Y, Y_type = Y_type)
    if (contrast_type == 'difference'){
      contrast_est <- Y_mean - Y_mean_2
    } else if (contrast_type == 'ratio'){
      contrast_est <- Y_mean / Y_mean_2
    } else if (contrast_type == 'none'){
      contrast_est <- NA
    }
  } else {
    Y_mean_2 <- contrast_est <- NA
    df_XW_2 <- NULL
  }

  out <- list(
    mean_est_1 = Y_mean,
    mean_est_2 = Y_mean_2,
    contrast_est = contrast_est,
    fit_Y = fit_Y, fit_W = fit_W,
    W_type = W_type, Y_type = Y_type,
    X_names = X_names,
    W_names = W_names,
    X_values_1 = X_values_1, X_values_2 = X_values_2,
    contrast_type = contrast_type,
    n_mc = n_mc,
    Y_model = Y_model,
    W_model = W_model,
    data = data
  )
  if (return_simulated_data){
    out$simulated_data <- list(
      sim_data_1 = df_XW,
      sim_data_2 = df_XW_2
    )
  }
  class(out) <- 'mia'
  return(out)
}

sim_W_all <- function(X_values, X_names, data_fit_W, fit, type, n_mc, W_names){
  df <- data.frame(matrix(rep(X_values, each = n_mc), ncol = length(X_values), byrow = FALSE))
  colnames(df) <- X_names
  for (i in 1:length(fit)){
    W_sim <- sim_W_individual(df = df, fit = fit[[i]], type = type[i], n_mc = n_mc,
                              levels = levels(data_fit_W[[W_names[i]]]))
    df[, W_names[i]] <- W_sim
  }
  return(df)
}

sim_W_individual <- function(df, fit, type, n_mc, levels){
  if (type == 'binary'){
    prob_W <- stats::predict(fit, type = 'response', newdata = df)
    W_sim <- stats::rbinom(n = n_mc, size = 1, prob = prob_W)
  } else if (type == 'categorical') {
    prob_W <- stats::predict(fit, type = 'probs', newdata = df)
    W_sim <- apply(prob_W, 1, function(probs) {
      sample(levels, size = 1, prob = probs)
    })
    W_sim <- factor(W_sim, levels = levels)
  } else if (type == 'normal'){
    mean_W <- stats::predict(fit, newdata = df)
    sd_W <- summary(fit)$sigma
    W_sim <- stats::rnorm(n = n_mc, mean = mean_W, sd = sd_W)
  }
  return(W_sim)
}

get_Y_mean <- function(df, fit_Y, Y_type){
  if (Y_type == 'binary'){
    Y_mean <- stats::predict(fit_Y, type = 'response', newdata = df)
  } else if (Y_type == 'continuous'){
    Y_mean <- stats::predict(fit_Y, newdata = df)
  }
  return(mean(Y_mean))
}

safe_fit <- function(variable_type, variable_name, formula, data) {
  fit <- tryCatch({
    if (variable_type == 'binary'){
      stats::glm(formula, data = data, family = stats::binomial())
    } else if (variable_type == 'categorical'){
      nnet::multinom(formula, data = data, trace = FALSE)
    } else if (variable_type %in% c('normal', 'continuous')){
      stats::lm(formula, data = data)
    }
  },
  error = function(e) {
    stop(paste0("Error in fitting the model for ", variable_name, ": ", conditionMessage(e)), call. = FALSE)
  }
  )
  return(fit)
}
