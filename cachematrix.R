## This file contains two functions: makeCacheMatrix and cacheSolve
## Overall goal of the two combined functions is to cache the inverse of a matrix

## makeCacheMatrix is designed to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inversion
## 4. get the value of the matrix inversion
## by making and returning a matrix object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve uses the matrix returned by makeCacheMatrix and computes its
## inverse. Before computing it checks to see if the value is already stored.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
