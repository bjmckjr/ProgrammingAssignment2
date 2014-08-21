## Put comments here that give an overall description of what your
## functions do

# These functions calculate the inverse of a matrix, using the Solve() function.
# The calculated inverse is cached, so that it only needs to be calculated once.
# Additional calls for this inverse will return the cached value.

## Write a short comment describing this function

# This function stores the original matrix in this environment, initializes the
# inverse to NUll (since it hasn't been calculated yet), and returns a list of
# functions to the calling procedure.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() {x}
      setinverse <- function(solve) {i <<- solve}
      getinverse <-function() {i}
      list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function

# This function takes a list of functions passed from the calling procedure,
# and uses them to 1) check if the inverse has been calculated and cached;
# 2) if it is cached, get and return the cached value; 3) if not cached,
# calculate the inverse and cache it.

cacheinverse <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}





