## Creates a system to access the inverse of a matrix without repeating calculations
##

## Creates a list of getting and setting functions for a matrix x and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # the inverse of x is unknown until calculated by cacheSolve
  set <- function(y) { # Only use set() to change your matrix, so the cached inverse is correct
    x <<- y # the <<- operator assures the x variable is changed in the function environment
    inv <<- NULL # a new matrix will generally have an uncalculated inverse
  }
  get <- function() x # provides a method to return the matrix originally put in
  set.inv <- function(i) inv <<- i # provides a method to set the inverse when calculated
  get.inv <- function() inv # provides a method to return the cached inverse
  
  list(set = set,
       get = get,
       set.inv = set.inv,
       get.inv = get.inv) # returns a named list of the desired functions
}


## Given the output of makeCacheMatrix, returns the inverse of the matrix
## and recalls the value from cache if it has already been calculated

cacheSolve <- function(x, ...) {
  inv <- x$get.inv()
  if(!is.null(inv)) { # if an inverse has been cached, return that inverse
    message("Getting cached data...")
    return(inv)
  }
  original.mat <- x$get() # otherwise, get the original matrix
  inv <- solve(original.mat, ...) # and calculate its inverse with any extra desired arguments
  x$set.inv(inv) # then cache the calculated inverse
  inv # and return the result
}
