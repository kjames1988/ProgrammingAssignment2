## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv<<- NULL
  }
  get <- function() x
  setsolve <- function(solve) Inv <<- solve
  getsolve <- function() Inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## This function computes the inverse of the special matrix returned by makeCacheMatrix above

cacheSolve  <- function(x, ...) {
  Inv <- x$getsolve()
  if(!is.null(Inv)) {
    message("getting cached matrix")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setsolve(Inv)
  Inv
}


