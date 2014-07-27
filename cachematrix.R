## These functions calculate the inverse of a given matrix and cache it. 
## If the inverse has already been calculated,it is returned from the cache
## rather than computing it again.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(newmat) {
    # by using <<- you can access to variables OUTSIDE of the function.
    # i.e. in an environment different from the one the function is in.
    mat <<- newmat
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(inversed.mat) inv <<- inversed.mat
  getinv <- function() inv
  
  # List of functions. Used in the other function to calculate an inversed matrix.
  list(set = set, get = get,
       setinv= setinv,
       getinv = getinv)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve gets the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Cached data found. \n Getting cached data.")
    return(inv)
  }
  message("No cached data found. \n Calculating and caching data...")
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}