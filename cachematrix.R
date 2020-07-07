## Put comments here that give an overall description of what your
## functions do
## This file has two functions makeCacheMatrix and cacheSolve. 
## Write a short comment describing this function
## The first function makeCacheMatrix() creates an R object and stores a vector and its matrix inverse.
## we should give input matrix as invertible 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
 ## The cacheSolve function() requires an arugument that is returned by makeCacheMatrix in order to retrieve the inverse from the cached value.


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()             
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

M<-matrix(c(1,0,0,0,1,0,0,0,1),3,3)
M1<-makeCacheMatrix(M)
cacheSolve(M1) ## inverse returned after the computation
cacheSolve(M1)

