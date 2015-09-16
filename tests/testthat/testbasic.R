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

    for (i in 1:2) {
        inv <- cacheSolve(cm)
        expect_is(inv, "matrix")
        expect_equal(nrow(inv), 5)

        product <- m %*% inv
        expect_equal(product, diag(N))
    }
})

test_that("Caching saves some time", {
    N <- 1000
    m <- matrix(rnorm(N*N), nrow = N)
    cm <- makeCacheMatrix(m)

    times <- lapply(as.list(1:2), function(i) {
        inv <- NULL
        t <- system.time(inv <- cacheSolve(cm))
        expect_is(inv, "matrix")
        expect_equal(nrow(inv), N)
        t
    })
    expect_true(times[[1]]["user.self"] > times[[2]]["user.self"]*100)
})
