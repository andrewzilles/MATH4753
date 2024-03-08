test_that("Test mu component of myncurve", {
  result <- myncurve(mu = 0, sigma = 1, a = 1)
  expect_equal(result$mu, 0)
})
test_that("Test sigma component of myncurve", {
  result <- myncurve(mu = 0, sigma = 1, a = 1)
  expect_equal(result$sigma, 1)
})
test_that("Test probability component of myncurve", {
  result <- myncurve(mu = 0, sigma = 1, a = 0)
  expect_equal(result$prob, 0.5)  # Expected value based on provided example
})
