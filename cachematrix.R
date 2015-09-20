## Functions for creating and using an object that encapsulates
## a matrix and its cached inverse.

## Make a CacheMatrix object from a matrix. The returned object
## has methods to get and set the matrix and the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function() x
    set <- function(m) {
        x <<- m
        inverse <<- NULL
    }
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Get the inverse of the matrix. If it has been cache, it is returned
## directly. Otherwise it is computed, cached, and returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        return (inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$setinverse(inv)
    inv
}
