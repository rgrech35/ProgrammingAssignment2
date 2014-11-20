# makeCacheMatrix creates a matrix object. If the matrix inverse has been calculated, 
# it will get it from the cache, otherwise cacheSolve will calculate it.

# Sample use scenario:
# > x = rbind(c(2, -1/2), c(-1/2, 1))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]  2.0 -0.5
# [2,] -0.5  1.0
# > cacheSolve(m)
# [,1]      [,2]
# [1,] 0.5714286 0.2857143
# [2,] 0.2857143 1.1428571
# > cacheSolve(m)
# getting cached data.
# [,1]      [,2]
# [1,] 0.5714286 0.2857143
# [2,] 0.2857143 1.1428571
 


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



# Return the inverse of the function created by makeCacheMatrix. If the cached version is available,
# it is retrieved from cache, otherwise it is calculated and returned.

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



