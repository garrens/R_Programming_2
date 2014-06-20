## These are a series of functions to implement matrices that cache the result
## of inversion calculations. If the matrix has not changed then the result of the
## previous calculation is returned and not calculated again.

## This is the top-level function that creates calculation chaching objects

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns a matrix that is the inverse of the argument matrix.
## If the calculation has already been done this function returns the saved
## result. If the calculagtion has not been done or the matrix has been changed
## since the previous calculation then the matrix is inverted. The result is
## both returned and saved for reuse if needed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- ginv(data, ...)
  x$setinv(i)
  i
}
