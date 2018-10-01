## Put comments here that give an overall description of what your
## functions do

#Matrix inversion is usually a costly computation and there may be some
#benefit to caching the inverse of a matrix rather than compute it repeatedly.
# This function will be used to compute the inverse of a matrix and cache the value of it.

## Write a short comment describing this function

# This function creates a special list which could be used to cache the inverse value.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

#This function calculates the inverse of a matrix created by the above function. And evaluate
#whether the inverse already exists. If so, it will retrieve the value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
