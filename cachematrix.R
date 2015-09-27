## Following pair of functions compute and cache the inverse of a matrix 
## so it does not need to be computed repeatedly.


# This file containts two functions: makeCacheMatrix and cacheSolve. 
# The first has the porpose of creating  a special "matrix" object that cache its inverse. 
# The second one computes the inverse of the special matrix returned by makeCacheMatrix.
# If the inverse has already been caculated, then the cacheSolve function
# just retrieve the inverse from the cache.


# Function: makeCacheMatrix
# Description: Store the matrix x an its inverse in the cache. 
# cache <- makeCacheMatrix(matrix) creates an initialized cache

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


# Function: cacheSolve (The inverse of the special "matrix" x)

# Description: Returns the inverse of the special "matrix" x. If the
# inverse has already been computed, it just returns it from the cache, 
# otherwise the inverse is computed and stored in the cache. 
# Input: A special "matrix" x (A list returnded by the makeCacheMatrix
# function)


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

