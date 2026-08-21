test_that("mia_nice point estimate unchanged: binary W, continuous Y", {
  set.seed(1234)
  res <- mia_nice(data = dat.sim,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1), X_values_2 = c(0, 0), contrast_type = 'none',
             Y_model = Y ~ W * X1 * X2, W_model = W ~ X1 * X2)

  expect_equal(res$mean_est_1, 1.977175, tolerance = 1e-5)
  expect_equal(res$mean_est_2, -0.03232477, tolerance = 1e-5)
})

test_that("mia_nice bootstrap CIs unchanged: binary W, continuous Y", {
  set.seed(1234)
  res <- mia_nice(data = dat.sim,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1), X_values_2 = c(0, 0),
             Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)
  set.seed(1234)
  res_ci <- get_CI(res, n_boot = 100, type = 'perc')

  expect_equal(res_ci$ci_1$percent[4], 2.039779, tolerance = 1e-5)
  expect_equal(res_ci$ci_1$percent[5], 2.247151, tolerance = 1e-5)
})

test_that("mia_nice point estimate unchanged: binary W, binary Y", {
  set.seed(1234)
  dat.sim_contY <- dat.sim
  dat.sim_contY$Y <- ifelse(dat.sim_contY$Y > median(dat.sim_contY$Y, na.rm = TRUE) / 2, 1, 0)

  res <- mia_nice(data = dat.sim_contY,
             X_names = c("X1", "X2"), X_values_1 = c(0, 1),
             Y_model = Y ~ W * X1 * X2, W_model = W ~ X1 * X2)

  expect_equal(res$mean_est_1, 0.4274451, tolerance = 1e-5)
})

test_that("mia point estimate unchanged: continuous W, continuous Y", {
  set.seed(1234)
  dat.sim_contW <- dat.sim
  dat.temp <- dat.sim_contW[!is.na(dat.sim_contW$W), ]
  dat.sim_contW[!is.na(dat.sim_contW$W), 'W'] <- rnorm(n = nrow(dat.temp), mean = dat.temp$W)

  res <- mia_nice(data = dat.sim_contW,
             X_names = c("X1", "X2"), X_values_1 = c(0, 1),
             Y_model = Y ~ W * X1 * X2, W_model = W ~ X1 * X2)

  expect_equal(res$mean_est_1, 1.995163, tolerance = 1e-5)
})

test_that("mia point estimate unchanged: categorical W, continuous Y", {
  set.seed(1234)
  dat.sim_catW <- dat.sim
  W1_ind <- which(dat.sim_catW$W == 1); W1_n <- length(W1_ind)
  dat.sim_catW[W1_ind[1:round(W1_n / 2)], 'W'] <- 2
  dat.sim_catW$W <- as.factor(dat.sim_catW$W)

  res <- mia_nice(data = dat.sim_catW,
             X_names = c("X1", "X2"), X_values_1 = c(0, 1),
             Y_model = Y ~ W * X1 * X2, W_model = W ~ X1 * X2)

  expect_equal(res$mean_est_1, 1.977191, tolerance = 1e-5)
})

test_that("mia point estimate unchanged: multivariate W, continuous Y", {
  set.seed(1234)
  dat.sim_multiW <- dat.sim
  dat.sim_multiW$W2 <- rnorm(nrow(dat.sim))
  res <- mia_nice(data = dat.sim_multiW,
             X_names = c("X1", "X2"),
             X_values_1 = c(0, 1), X_values_2 = c(0, 0), contrast_type = 'none',
             Y_model = Y ~ W * X1 * X2,
             W_model = list(W ~ X1 * X2, W2 ~ W + X1 + X2))

  expect_equal(res$mean_est_1, 1.976322, tolerance = 1e-5)
  expect_equal(res$mean_est_2, -0.03241571, tolerance = 1e-5)
})

################################################################################
# Comparing to Maya's code: binary W, continuous Y
################################################################################

# For simplicity, let's regenerate the dataset with original column names
expit = function(p) exp(p) / (1 + exp(p))
N <- 10000
set.seed(1234)
du <- data.frame(C1 = rbinom(n = N, size = 1,  prob = 0.5))
du$A1 <- rbinom(n = N, size = 1, prob = expit(-1 + 3*du$C1))
du$D1 <- rbinom(n = N, size = 1, prob = expit(-1 + 3*du$A1))
du$B1 <- rnorm(n = N, mean = 2.6*du$C1 + 2*du$A1 + du$A1*du$C1)

du$RA <- rbinom(n = N, size = 1, prob = expit(-1 + 3*du$D1))
du$RD <- rbinom(n = N, size = 1, prob = 0.5)
du$RC <- rbinom(n = N, size = 1, prob = expit(-1 + 3*du$D1))
du$RB <- rbinom(n = N, size = 1, prob = expit(-1 + 3*du$D1))

du$A <- ifelse(du$RA == 1, du$A1, NA)
du$B <- ifelse(du$RB == 1, du$B1, NA)
du$C <- ifelse(du$RC == 1, du$C1, NA)
du$D <- ifelse(du$RD == 1, du$D1, NA)

a <- 1; c <- 0
dc = du[ complete.cases(du), ]
dw = du[du$RA == 1 & du$RD == 1 & du$RC == 1, ]

# 'Nonparametric' method
est = sum( sapply(0:1, function(d) {
  p_d1 <- mean( dw$D[ dw$A == a & dw$C == c ] == 1 )
  if (d == 1) p_d = p_d1 else p_d = 1 - p_d1
  mu  = mean( dc$B[ dc$A == a & dc$C == c & dc$D == d ] )
  p_d * mu
} ) )
est # 1.976309

# 'Semiparametric' method
fit_B <- lm( B ~ A * C * D, data = dc )
fit_D <- glm(D ~ A * C, data = dw, family = binomial)
est = sum( sapply(0:1, function(d) {
  p_d1 <- predict(fit_D, newdata = data.frame(A=a, C=c), type="response")
  if (d == 1) p_d = p_d1 else p_d = 1 - p_d1
  mu  <- predict(fit_B, newdata = data.frame(A=a, C=c, D=d))
  p_d * mu
} ) )
est # 1.976309
