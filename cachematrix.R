## This function reates a cache version copy of the original matrix.
## The implementation is mimicked from the makeVector code.
## In comparison, changed 'mean' function references to 'inverse'
## This is comparable to a class with 4 methods (get, set, getSolved, setSolved)
## Test case used:
## M <- matrix(rnorm(9),3,3); # randomized square matrix (high chance of reversibility)
## T <- makeCacheMatrix(M) --or-- T <- makeCacheMatrix(); T$set(M)
## cacheSolve(T)      # creating cache (testing if value is created)
## cacheSolve(T) # getting cache (testing if retrieved from cache)
## round(T$get() %*% cacheSolve(T))     # results in an identity matrix (testing if true inverse)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  # the code below is basically API for cached matrix object
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function retrieves cached inverse (creates it first if doesn't exist)
## A special check for invalid argument included, 
## this function will not work with regular matrices.
## With valid argument, the function checks if a cached value exists.
## If it doesn't exist, creates the inverse (solution).
cacheSolve <- function(x, ...) {
  if(is.atomic(x) || is.null(x$setInverse)) {
    message("argumnet invalid, not a cached matrix")
    return(NULL)
  }
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("creating cached data")
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
