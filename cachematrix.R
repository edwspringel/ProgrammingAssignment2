## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

### ASSIGNMENT: write a pair of functions that cache the 
##inverse of a matrix
## the function solve(X) where X is a square invertable
## matrix

## The first function set a matrix value, gets it, then sets the matrix value. 
## Then it sets its inverse, and gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##need to create a matrix object x
  ##create a cache m
  
  m <- NULL
  set <- function(y) {
    x <<- y
    ## sends a matrix y to the object x within the
    ## parent enviroment, but you need to make m
    ## NULL in the parent enviroment
    m <<- NULL
  }
  get <- function() x
  setSolveInverse <- function(inverse) m <<- inverse
  getSolveInverse <- function() m  ##gets the cached inverse of the matrix x
  list(set = set, get = get,
       setSolveInverse = setSolveInverse,
       getSolveInverse = getSolveInverse)
}

## This function is written to the inverse of the matrix as created with 
##the prior function. Checking to see if inverse is already present in cache.
##If it's there then the cache is used, else it uses the function I wrote above
##to calculate the inverse and put it into the cache.

cacheSolve <- function(x, ...) {  ##gets the inverse of the matrix x
  m <- x$getSolveInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setSolveInverse(m)
  m
}
