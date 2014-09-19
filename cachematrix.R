## cacheMatrix.R is a set of functions that 
## take a matrix passed in and initially
## calculate and cache it's inverse such that
## the inverse is calculated only once and 
## only the cached inverse is used if needed
## again later.


## The makeCacheMatrix function creates an  
## object that can cache the inverse of the 
## matrix passed in when called by the 
## cacheSolve function

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



## cacheSolve function returns the inverse of
## a matrix passed.  If it does not already
## exist in cache, it calculates and caches
## the inverse.  If the inverse is cached,
## it returns the cached value only.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
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
