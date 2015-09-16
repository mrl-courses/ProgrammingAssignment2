source("../../cachematrix.R")

test_that("Basic methods are present", {
  m <- matrix(nrow=3, ncol=3)
  cm <- makeCacheMatrix(m)
  expect_true(is.function(cm$get))
  expect_true(is.function(cm$set))
  expect_true(is.function(cm$getinverse))
  expect_true(is.function(cm$getinverse))
})

test_that("Basic methods appear to work", {
    N <- 5
    m <- matrix(rnorm(N*N), nrow = N)
    cm <- makeCacheMatrix(m)
    inv <- cacheSolve(cm)
    expect_is(inv, "matrix")
    expect_equal(nrow(inv), 5)

    product <- m %*% inv
    expect_equal(product, diag(N))
})
