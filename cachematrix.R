## R Functions to cache inverse of a matrix and calculate inverse if no cache
## is available.  Functions assume a square matrix is passed in.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## Borrowed logic from makeVector function example provided in Coursera R course
## and adjusted to use matrix/solve instead of vector/mean
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## Borrowed logic from cachemean function example provided in Coursera R course
## and replaced 'mean' with 'solve'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
