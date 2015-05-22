## This file contains two functions hat are used to create 
## a special object that stores a matrix and cache's its inverse


## this function (makeCacheMatrix) creates a special "matrix", which is 
## in fact a list containing a function to set/get the matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function returns the cached inverse of the special "matrix"
## created with makeCacheMatrix() above. 
## If there is no cached copy of the inverse, then this function computes inverse before returning it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
