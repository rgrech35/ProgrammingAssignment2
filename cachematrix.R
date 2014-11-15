## Put comments here that give an overall description of what your
## functions do

## This takes values passed to it and stores them as a matrix. If when CacheSolve is called and the matrix 
## inverse is not cached, the function will take the matrix passed from cacheSolve and calculate 
## the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}




## This function checks to see if the matrix inverse has been stored in cache. If not, the inverse
## is calculated. If not, it is calculated and stored in cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}