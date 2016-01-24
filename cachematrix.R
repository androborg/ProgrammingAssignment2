# The functions 'makeCacheMatrix' and 'cacheSolve' are used to cache the inverse of a matrix.

# 'makeCacheMatrix' returns a list containing a function to set and get the value of the matrix.
# The function also sets and gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. 
# The function,
# 1. checks if the inverse has already been computed. If so, it gets the result and skips the
# computation. 
# 2. If not, it computes the inverse, sets the value in the cache with setinverse function and returns 
# the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
    
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv   ## matrix which is the inverse of 'x' is returned here
}