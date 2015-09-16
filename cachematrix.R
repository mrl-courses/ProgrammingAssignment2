## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
