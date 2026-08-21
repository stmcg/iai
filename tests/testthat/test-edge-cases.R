# Tests for edge cases and special scenarios

test_that("mia_nice works with single predictor", {
  set.seed(1234)
  dat_singleX <- dat.sim[, c("Y", "X1", "W")]

  res <- mia_nice(data = dat_singleX,
             X_names = "X1",
             X_values_1 = 1,
             Y_model = Y ~ W + X1, W_model = W ~ X1)

  expect_true(!is.na(res$mean_est_1))
  expect_true(is.numeric(res$mean_est_1))
  expect_equal(res$contrast_type, 'none')
})

test_that("mia_nice works with return_simulated_data = TRUE", {
  set.seed(1234)
  res <- mia_nice(data = dat.sim,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1), X_values_2 = c(0, 0),
             Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2,
             return_simulated_data = TRUE,
             n_mc = 100)  # Smaller for speed

  expect_true(!is.null(res$simulated_data))
  expect_true(is.list(res$simulated_data))
  expect_true("sim_data_1" %in% names(res$simulated_data))
  expect_true("sim_data_2" %in% names(res$simulated_data))
  expect_equal(nrow(res$simulated_data$sim_data_1), 100)
  expect_equal(nrow(res$simulated_data$sim_data_2), 100)
})


test_that("mia_nice handles categorical W with explicit W_type", {
  set.seed(1234)
  dat_catW <- dat.sim
  W1_ind <- which(dat_catW$W == 1)
  W1_n <- length(W1_ind)
  dat_catW[W1_ind[1:round(W1_n / 2)], 'W'] <- 2
  dat_catW$W <- as.factor(dat_catW$W)

  res <- mia_nice(data = dat_catW,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1),
             Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2,
             W_type = "categorical")

  expect_true(!is.na(res$mean_est_1))
  expect_equal(res$W_type, "categorical")
})

test_that("mia_nice handles binary Y with explicit Y_type", {
  set.seed(1234)
  dat_binY <- dat.sim
  dat_binY$Y <- ifelse(dat_binY$Y > median(dat_binY$Y, na.rm = TRUE), 1, 0)

  res <- mia_nice(data = dat_binY,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1),
             Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2,
             Y_type = "binary")

  expect_true(!is.na(res$mean_est_1))
  expect_equal(res$Y_type, "binary")
  # Binary outcome mean should be between 0 and 1
  expect_true(res$mean_est_1 >= 0 && res$mean_est_1 <= 1)
})

test_that("mia_nice works with only X_values_1 (no contrast)", {
  set.seed(1234)
  res <- mia_nice(data = dat.sim,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1),
             # No X_values_2
             Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)

  expect_true(!is.na(res$mean_est_1))
  expect_true(is.na(res$mean_est_2))
  expect_true(is.na(res$contrast_est))
  expect_equal(res$contrast_type, 'none')
})



test_that("mia_nice print methods work correctly", {
  set.seed(1234)
  res <- mia_nice(data = dat.sim,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1), X_values_2 = c(0, 0),
             Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)

  # Should not error
  expect_output(print(res), "MIA METHOD")

  set.seed(1234)
  res_ci <- get_CI(res, n_boot = 50, type = 'perc')
  expect_output(print(res_ci), "BOOTSTRAP CONFIDENCE INTERVALS")
})

