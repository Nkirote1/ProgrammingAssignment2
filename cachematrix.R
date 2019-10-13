## Caching a Matrix Inverse
## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cont <- NULL
  set <- function(y) {
    x <<- y
    cont<<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cont <<- inverse
  getInverse <- function() cont
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## the makeCacheMatrix. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cont <- x$getInverse()
  if (!is.null(cont)) {
    message("getting cached data")
    return(cont)
  }
  mat <- x$get()
  cont <- solve(mat, ...)
  x$setInverse(cont)
  cont
}