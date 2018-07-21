## Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than computing it repeatedly 
##(there are also alternatives to matrix inversion that we will not discuss here). 
## Below code is to write a pair of functions that cache the inverse of a matrix.

## MakeCacheMatrix is a function that does 4 things
## set the value of the matrix
## get the value of the matrix
## set the value of the solve
## get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the solve of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the solve has already been calculated. If so, it gets the solve 
## from the cache and skips the computation. Otherwise,
## it calculates the solve of the data and sets the value of the mean 
## in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
