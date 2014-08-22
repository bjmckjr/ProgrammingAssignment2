## Put comments here that give an overall description of what your
## functions do

# These functions calculate the inverse of a matrix, using the Solve() function.
# The calculated inverse is cached, so that it only needs to be calculated once.
# Thereafter, additional calls for the inverse will return the cached value.

## Write a short comment describing this function

# This function initializes the object that will cache the inverse, and returns a
# list of supported functions to the calling procedure.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL  # Initialize the inverse variable to NULL until requested.
      set <- function(y) {   # Function to store the initial matrix.
            x <<- y
            i <<- NULL
      }
      get <- function() {x} # Function to return the initial matrix.
      setinverse <- function(solve) {i <<- solve} # FUnction to calculate inverse matrix.
      getinverse <-function() {i} # Function to return the inverse matrix
      list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function

# This function takes an object (list of functions) passed from the calling procedure,
# and uses them to 1) check if the inverse has been calculated and cached;
# 2) if it is cached, get and return the cached value; 3) if not cached,
# calculate the inverse and cache it.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
      i <- x$getinverse() # Request for the inverse from the cache object.
      if(!is.null(i)) {  # Check if inverse is not NULL(has previously been calculated).
            message("getting cached data") # If not NULL, print message
            return(i)  # and return cached Inverse matrix.
      }
      data <- x$get() # If the inverse is NULL, get original Matrix...
      i <- solve(data, ...)  # and calculate the inverse matrix.
      x$setinverse(i) # Cache the inverse so it will be available for future requests.
      i  # Return the calculated inverse the first time.
}





