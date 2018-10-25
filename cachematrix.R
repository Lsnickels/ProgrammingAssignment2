##These functions will solve for the inverse of a given square matrix and store the result in the cache.
##To use these functions, first pass the square matrix into the makeCacheMatrix function and store it in a variable.
#Then, pass the variable into the second function, cacheSolve. This will solve the matrixfor its inverse, and store the
#result in the cache for later use.

## This first function sets up the list of functions for getting and setting the inverse matrix. It also formats the 
##data for use in the second matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(result) m <<- result
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will return the inverse matrix based on the result from the first function. If it has already
##solved it, it will pass the cached solution.

cacheSolve <- function(x, ...) {
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
