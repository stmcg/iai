# Tests for contrast types and related functionality

test_that("mia_nice ratio contrast works correctly", {
  set.seed(1234)
  res <- mia_nice(data = dat.sim,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1), X_values_2 = c(0, 0),
             contrast_type = 'ratio',
             Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)

  expect_true(!is.na(res$contrast_est))
  expect_true(is.numeric(res$contrast_est))
  expect_equal(res$contrast_type, 'ratio')
  # Ratio should be mean_est_1 / mean_est_2
  expect_equal(res$contrast_est, res$mean_est_1 / res$mean_est_2, tolerance = 1e-10)
})

test_that("mia_nice difference contrast works correctly", {
  set.seed(1234)
  res <- mia_nice(data = dat.sim,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1), X_values_2 = c(0, 0),
             contrast_type = 'difference',
             Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)

  expect_true(!is.na(res$contrast_est))
  expect_true(is.numeric(res$contrast_est))
  expect_equal(res$contrast_type, 'difference')
  # Difference should be mean_est_1 - mean_est_2
  expect_equal(res$contrast_est, res$mean_est_1 - res$mean_est_2, tolerance = 1e-10)
})

test_that("mia_nice contrast_type = 'none' with X_values_2", {
  set.seed(1234)
  res <- mia_nice(data = dat.sim,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1), X_values_2 = c(0, 0),
             contrast_type = 'none',
             Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)

  expect_true(!is.na(res$mean_est_1))
  expect_true(!is.na(res$mean_est_2))
  expect_true(is.na(res$contrast_est))
  expect_equal(res$contrast_type, 'none')
})

test_that("mia_nice default contrast_type when X_values_2 provided", {
  set.seed(1234)
  res <- mia_nice(data = dat.sim,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1), X_values_2 = c(0, 0),
             # contrast_type not specified
             Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)

  expect_equal(res$contrast_type, 'difference')
  expect_true(!is.na(res$contrast_est))
})

test_that("mia_nice bootstrap CI works with ratio contrast", {
  set.seed(1234)
  res <- mia_nice(data = dat.sim,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1), X_values_2 = c(0, 0),
             contrast_type = 'ratio',
             Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)

  set.seed(1234)
  res_ci <- get_CI(res, n_boot = 50, type = 'perc')

  expect_true(!is.null(res_ci$ci_contrast))
  expect_true(inherits(res_ci$ci_contrast, "bootci"))
})

test_that("mia_nice bootstrap CI works with difference contrast", {
  set.seed(1234)
  res <- mia_nice(data = dat.sim,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1), X_values_2 = c(0, 0),
             contrast_type = 'difference',
             Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)

  set.seed(1234)
  res_ci <- get_CI(res, n_boot = 50, type = 'perc')

  expect_true(!is.null(res_ci$ci_contrast))
  expect_true(inherits(res_ci$ci_contrast, "bootci"))
})

test_that("mia_nice bootstrap CI with contrast_type = 'none'", {
  set.seed(1234)
  res <- mia_nice(data = dat.sim,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1), X_values_2 = c(0, 0),
             contrast_type = 'none',
             Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)

  set.seed(1234)
  res_ci <- get_CI(res, n_boot = 50, type = 'perc')

  expect_true(!is.null(res_ci$ci_1))
  expect_true(!is.null(res_ci$ci_2))
  expect_true(is.null(res_ci$ci_contrast))  # No contrast CI when contrast_type = 'none'
})

