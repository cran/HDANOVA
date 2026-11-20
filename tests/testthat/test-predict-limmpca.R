test_that("predict.limmpca returns limmpca object", {
  data(candies)

  test_idx <- seq(3, nrow(candies), by = 3)
  train_idx <- setdiff(seq_len(nrow(candies)), test_idx)
  candies_train <- candies[train_idx, ]
  candies_test <- candies[test_idx, ]

  mod <- limmpca(assessment ~ candy + r(assessor),
                 data = candies_train,
                 pca.in = 3,
                 REML = TRUE)

  pred <- predict(mod, newdata = candies_test)

  expect_true(inherits(pred, "limmpca"))
  expect_true(inherits(pred, "asca"))
  expect_equal(nrow(pred$Y), nrow(candies_test))
})

test_that("REML limmpca handles interaction and combined random effects without QR failure", {
  data(candies)
  mod <- suppressMessages(limmpca(assessment ~ candy * r(assessor), data = candies))
  expect_true(inherits(mod, "limmpca"))
  expect_true(all(is.finite(mod$ssq)))
  expect_equal(length(mod$anovas), ncol(mod$Y_fit))

  data(caldana)
  mod_comb <- suppressMessages(
    limmpca(compounds ~ time + comb(r(light) + r(time:light)), data = caldana, pca.in = 8)
  )
  expect_true(inherits(mod_comb, "limmpca"))
  expect_true("light+time:light" %in% names(mod_comb$ssq))
  expect_true(all(is.finite(mod_comb$ssq)))
})
