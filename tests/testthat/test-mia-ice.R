test_that("mia_ice point estimate matches saturated cell mean: binary W, continuous Y", {
  set.seed(1234)
  res <- mia_ice(data = dat.sim,
                 X_names = c("X1", "X2"),
                 X_values_1 = c(0, 1), X_values_2 = c(0, 0), contrast_type = 'none',
                 Y_model = Y ~ W * X1 * X2, outer_model = g_hat ~ X1 * X2)

  expect_equal(res$mean_est_1, 1.976309306, tolerance = 1e-5)
  expect_equal(res$mean_est_2, -0.03225690983, tolerance = 1e-5)
})

test_that("mia_ice with saturated outer model equals the reference cell mean", {
  # With a saturated outer model, mia_ice must reproduce the sample mean of the
  # fitted outcome means among units with X = X_values and R_X = R_W = 1.
  a <- 1; c <- 0  # target X2 = 1, X1 = 0
  dc <- dat.sim[stats::complete.cases(dat.sim), ]
  dw <- dat.sim[stats::complete.cases(dat.sim[, c("X1", "X2", "W")]), ]

  fit_g <- stats::lm(Y ~ W * X1 * X2, data = dc)
  dw_pred <- dw; dw_pred$X1 <- c; dw_pred$X2 <- a
  g_hat <- stats::predict(fit_g, newdata = dw_pred)
  ref <- mean(g_hat[dw$X1 == c & dw$X2 == a])

  res <- mia_ice(data = dat.sim,
                 X_names = c("X1", "X2"), X_values_1 = c(0, 1),
                 Y_model = Y ~ W * X1 * X2, outer_model = g_hat ~ X1 * X2)

  expect_equal(res$mean_est_1, ref, tolerance = 1e-8)
})

test_that("mia_ice point estimate unchanged: binary W, binary Y", {
  set.seed(1234)
  dat.sim_binY <- dat.sim
  dat.sim_binY$Y <- ifelse(dat.sim_binY$Y > median(dat.sim_binY$Y, na.rm = TRUE) / 2, 1, 0)

  res <- mia_ice(data = dat.sim_binY,
                 X_names = c("X1", "X2"), X_values_1 = c(0, 1),
                 Y_model = Y ~ W * X1 * X2, outer_model = g_hat ~ X1 * X2)

  expect_equal(res$mean_est_1, 0.4270914603, tolerance = 1e-5)
})

test_that("mia_ice point estimate unchanged: categorical W, continuous Y", {
  set.seed(1234)
  dat.sim_catW <- dat.sim
  W1_ind <- which(dat.sim_catW$W == 1); W1_n <- length(W1_ind)
  dat.sim_catW[W1_ind[1:round(W1_n / 2)], 'W'] <- 2
  dat.sim_catW$W <- as.factor(dat.sim_catW$W)

  res <- mia_ice(data = dat.sim_catW,
                 X_names = c("X1", "X2"), X_values_1 = c(0, 1),
                 Y_model = Y ~ W * X1 * X2, outer_model = g_hat ~ X1 * X2)

  expect_equal(res$mean_est_1, 1.976322892, tolerance = 1e-5)
})

test_that("mia_ice contrast equals difference of means", {
  set.seed(1234)
  res <- mia_ice(data = dat.sim,
                 X_names = c("X1", "X2"),
                 X_values_1 = c(0, 1), X_values_2 = c(0, 0), contrast_type = 'difference',
                 Y_model = Y ~ W * X1 * X2, outer_model = g_hat ~ X1 * X2)

  expect_equal(res$contrast_est, res$mean_est_1 - res$mean_est_2, tolerance = 1e-10)
})

test_that("mia_ice errors on missing outer_model", {
  expect_error(
    mia_ice(data = dat.sim,
            X_names = c("X1", "X2"), X_values_1 = c(0, 1),
            Y_model = Y ~ W * X1 * X2),
    "outer_model must be supplied"
  )
})

test_that("mia_ice errors on outer_model with wrong LHS", {
  expect_error(
    mia_ice(data = dat.sim,
            X_names = c("X1", "X2"), X_values_1 = c(0, 1),
            Y_model = Y ~ W * X1 * X2, outer_model = mean ~ X1 * X2),
    "The left-hand side of outer_model must be 'g_hat'"
  )
})

test_that("mia_ice errors when outer_model RHS is not in X_names", {
  expect_error(
    mia_ice(data = dat.sim,
            X_names = c("X1", "X2"), X_values_1 = c(0, 1),
            Y_model = Y ~ W * X1 * X2, outer_model = g_hat ~ W),
    "The right-hand side of outer_model can only depend on the predictor"
  )
})

test_that("get_CI works on a mia_ice object", {
  set.seed(1234)
  res <- mia_ice(data = dat.sim,
                 X_names = c("X1", "X2"), X_values_1 = c(0, 1),
                 Y_model = Y ~ W + X1 + X2, outer_model = g_hat ~ X1 * X2)
  res_ci <- get_CI(res, n_boot = 50, type = 'perc', show_progress = FALSE)

  expect_true(is.finite(res_ci$ci_1$percent[4]))
  expect_true(is.finite(res_ci$ci_1$percent[5]))
})
