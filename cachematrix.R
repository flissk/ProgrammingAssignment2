## makeCacheMatrix creates a matrix that can cache the inverse.
## cacheSolve computes or retrieves inverse from the cache returned by makeCacheMatrix. 

## Write a short comment describing this function
## makeCacheMatrix creates a matrix object that can cache its inverse.

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


## Write a short comment describing this function
# cacheSolve computes the inverse of matrix returned by makeCacheMatrix. 
# If the inverse has been calculated, cachesolve retrieves inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}



