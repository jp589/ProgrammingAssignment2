## These functions take a matrix, cache it, and return the matrix from the cache.

## makeCacheMatrix takes an invertible matrix as its argument and returns a list object where each element of the list
## is a function that can be called on the invertible matrix. It can set the matrix, get the matrix, invert the matrix,
## and get the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  invmatrix <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, invmatrix = invmatrix, getinv = getinv)
}


## cacheSolve is a function that takes an object of type makeCacheMatrix() and gets the cached matrix. It then inverts
## the cached matrix and returns it.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$invmatrix(m)
  m
}