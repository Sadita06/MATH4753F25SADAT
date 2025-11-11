test_that("myncurve function works correctly", {
  # Test basic functionality
  result <- myncurve(mu = 10, sigma = 5, a = 6)

  # Test that it returns a list
  expect_type(result, "list")

  # Test that list has correct names
  expect_named(result, c("mu", "sigma", "probability", "a"))

  # Test that parameters are stored correctly
  expect_equal(result$mu, 10)
  expect_equal(result$sigma, 5)
  expect_equal(result$a, 6)

  # Test that probability is calculated correctly
  expected_prob <- pnorm(6, mean = 10, sd = 5)
  expect_equal(result$probability, round(expected_prob, 4))
})
