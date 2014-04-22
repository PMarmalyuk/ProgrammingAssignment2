# Hello, world!
## This R file contains definitions of two functions: makeCacheMatrix and cacheSolve.
## They allow to cache a result of potentially time-consuming operation of matrix inversion and prevent recomputing.
## Matrix inversion is calculated with aid of solve() built-in R function.
## These functions have been written in accordance with instructions in the programming assignment: 
## see https://github.com/PMarmalyuk/ProgrammingAssignment2

### This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

### cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
### If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
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