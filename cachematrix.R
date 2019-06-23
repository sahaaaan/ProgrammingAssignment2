## Put comments here that give an overall description of what your
## functions do
##The aim of this exercise is to write to functions to cache the inverse of a matrix.

## Write a short comment describing this function
## The function "makeCacheMatric" creates a special "matrix" object that can cache its inverse (invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by the function "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix has not changed), 
## then the function "cacheSolve" retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
