library("testthat")
context("Calculate the inverse of a square matrix")

testData <- function(x = matrix()) {
  inverseX <- solve(x)
  y <- makeCacheMatrix(x)  
  list(x = x, inverseX = inverseX, y = y)
}

test_that("match the example output for this function", {
  data <- testData(matrix(1:4, 2, 2))
  expect_equal(cacheSolve(data$y), data$inverseX)
  expect_equal(data$y$getHits(), 0)
  expect_equal(cacheSolve(data$y), data$inverseX)
  expect_equal(data$y$getHits(), 1)
  
  data <- testData(matrix(rnorm(16 * 16), 16, 16))
  expect_equal(cacheSolve(data$y), data$inverseX)
  expect_equal(cacheSolve(data$y), data$inverseX)
  expect_equal(cacheSolve(data$y), data$inverseX)
  expect_equal(data$y$getHits(), 2)
})

