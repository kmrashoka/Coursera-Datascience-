# this function holds the cached value or NULL if nothing is cached
# initially nothing is cached so set it to NULL
makeCacheMatrix <- function(x = numeric()) {
  cache <- NULL
  # store a matrix since the matrix is assigned a new value, flush the cache
  setMatrix <- function(newValue) {
    x <<- newValue
    cache <<- NULL
  }
  
  # Display the stored matrix
  getMatrix <- function() {
    x
  }
  
  # cache the given argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       cacheInverse = cacheInverse, 
       getInverse = getInverse)
}