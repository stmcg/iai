# Tests for error handling and edge cases

test_that("mia_nice errors on missing Y column", {
  dat_noY <- dat.sim[, c("X1", "X2", "W")]
  expect_error(
    mia_nice(data = dat_noY,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2),
    "The observed data must include a column called 'Y'"
  )
})

test_that("mia_nice errors on missing X_names columns", {
  expect_error(
    mia_nice(data = dat.sim,
        X_names = c("X1", "X3"),  # X3 doesn't exist
        X_values_1 = c(0, 1),
        Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2),
    "The following columns are listed in X_names but missing from the data"
  )
})

test_that("mia_nice errors on invalid Y_model (not a formula)", {
  expect_error(
    mia_nice(data = dat.sim,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = "Y ~ W + X1 + X2", W_model = W ~ X1 + X2),
    "Y_model must be a formula"
  )
})

test_that("mia_nice errors on Y_model with wrong LHS", {
  expect_error(
    mia_nice(data = dat.sim,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = Outcome ~ W + X1 + X2, W_model = W ~ X1 + X2),
    "The left-hand side of Y_model must be the variable 'Y'"
  )
})

test_that("mia_nice errors on invalid W_model (not formula or list)", {
  expect_error(
    mia_nice(data = dat.sim,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = Y ~ W + X1 + X2, W_model = "W ~ X1 + X2"),
    "W_model must be a formula"
  )
})

test_that("mia_nice errors on W_model list with non-formula element", {
  expect_error(
    mia_nice(data = dat.sim,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = Y ~ W + X1 + X2, W_model = list(W ~ X1 + X2, "invalid")),
    "Element 2 in W_model is not a formula"
  )
})

test_that("mia_nice errors on missing W column in data", {
  dat_noW <- dat.sim[, c("Y", "X1", "X2")]
  expect_error(
    mia_nice(data = dat_noW,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2),
    "The observed data must include a column called W"
  )
})

test_that("mia_nice errors on X_values length mismatch", {
  expect_error(
    mia_nice(data = dat.sim,
        X_names = c("X1", "X2"),
        X_values_1 = c(0),  # Wrong length
        Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2),
    "The arguments 'X_names' and 'X_values_1' must be of the same length"
  )
})

test_that("mia_nice errors on invalid X_values for categorical predictors", {
  dat_catX <- dat.sim
  dat_catX$X1 <- factor(dat_catX$X1, levels = c(0, 1))

  expect_error(
    mia_nice(data = dat_catX,
        X_names = c("X1", "X2"),
        X_values_1 = c(2, 1),  # 2 is not a valid level
        Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2),
    "The value 2 specified for predictor 'X1' in X_values_1 is not a valid level"
  )
})

test_that("mia_nice errors on empty data_fit_Y", {
  dat_all_missing <- dat.sim
  dat_all_missing$Y <- NA  # All Y missing

  expect_error(
    mia_nice(data = dat_all_missing,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2),
    "No complete cases found for fitting the outcome model"
  )
})

test_that("mia_nice errors on empty data_fit_W", {
  # Create scenario where W is missing for all cases with complete X
  # This will make data_fit_W empty
  dat_missing_W <- dat.sim
  # Make W missing for all cases where X is complete
  complete_X <- complete.cases(dat_missing_W[, c("X1", "X2")])
  dat_missing_W$W[complete_X] <- NA

  expect_error(
    mia_nice(data = dat_missing_W,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2),
    "No complete cases found"
  )
})

test_that("mia_nice errors on type inference failure for W", {
  dat_unknown <- dat.sim
  # Create a character variable that can't be inferred (not binary, not factor, not numeric)
  dat_unknown$W <- paste0("level_", dat_unknown$W)
  dat_unknown$W <- as.character(dat_unknown$W)  # Character with multiple unique values, not inferrable

  expect_error(
    mia_nice(data = dat_unknown,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2),
    "Unable to infer the type for auxiliary variable"
  )
})

test_that("mia_nice errors on type inference failure for Y", {
  dat_unknown <- dat.sim
  dat_unknown$Y <- as.character(dat_unknown$Y)  # Character, not inferrable

  expect_error(
    mia_nice(data = dat_unknown,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2),
    "Unable to infer the type for the outcome variable Y"
  )
})

test_that("mia_nice errors on invalid W_type", {
  expect_error(
    mia_nice(data = dat.sim,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2,
        W_type = "invalid"),
    "W_type must be set to either 'binary', 'categorical', or 'normal'"
  )
})

test_that("mia_nice errors on invalid Y_type", {
  expect_error(
    mia_nice(data = dat.sim,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2,
        Y_type = "invalid"),
    "Y_type must be set to either 'binary' or 'continuous'"
  )
})

test_that("mia_nice errors on W_model dependency order violation", {
  dat_multiW <- dat.sim
  dat_multiW$W2 <- rnorm(nrow(dat_multiW))

  # W2 depends on W1, but W1 is listed first - this should work
  # But if we reverse the order incorrectly, it should fail
  expect_error(
    mia_nice(data = dat_multiW,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = Y ~ W + W2 + X1 + X2,
        W_model = list(W2 ~ W + X1 + X2, W ~ X1 + X2)),  # Wrong order
    "depends on the following component"
  )
})

test_that("mia_nice errors on categorical W without factor encoding", {
  dat_catW <- dat.sim
  # Create a categorical variable with 3+ levels that's not a factor
  dat_catW$W <- ifelse(dat_catW$W == 0, "A",
                       ifelse(dat_catW$W == 1, "B", "C"))
  dat_catW$W[1:100] <- "A"
  dat_catW$W[101:200] <- "B"
  dat_catW$W[201:300] <- "C"
  dat_catW$W <- as.character(dat_catW$W)  # Not a factor, but has 3+ levels

  expect_error(
    mia_nice(data = dat_catW,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2,
        W_type = "categorical"),
    "Auxiliary variables with type 'categorical' need to be encoded as factors"
  )
})

test_that("mia_nice errors on binary W specified as categorical", {
  expect_error(
    mia_nice(data = dat.sim,
        X_names = c("X1", "X2"),
        X_values_1 = c(0, 1),
        Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2,
        W_type = "categorical"),
    "W_type should be set to 'binary' when W only has two levels"
  )
})

