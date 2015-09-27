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
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


# Function: cacheSolve (The inverse of the special "matrix" x)

# Description: Returns the inverse of the special "matrix" x. If the
# inverse has already been computed, it just returns it from the cache, 
# otherwise the inverse is computed and stored in the cache. 
# Input: A special "matrix" x (A list returnded by the makeCacheMatrix
# function)


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
