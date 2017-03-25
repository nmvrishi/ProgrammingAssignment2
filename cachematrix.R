## The file has two functions
## makeCacheMatrix and cacheSolve that stores the inverse of a matrix in cache

## This function takes a square matrix and stores the inverse of that square matrix. It does not calculate the inverse.
## It exposes get, set, getInverse, setInverse methods to get and set the square matrix and its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(matinv) inv <<- matinv
  getInverse <- function() inv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function takes a function that stores the inverse of the square matrix. 
## For a given square matrix, the function checks if the invrese is available in cache.
## If available, it returns the inverse from cache. 
## If not, it calculates the inverse and stores it in the function's cache for future use.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
