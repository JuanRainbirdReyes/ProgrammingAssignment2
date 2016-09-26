## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## The next pair of functions cache the inverse of a matrix.

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      minverse <- NULL
      set <- function(y) {
            x <<- y
            minverse <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) minverse <<- solve
      getinverse <- function() minverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
      minverse <- x$getinverse()
      if(!is.null(minverse)) {
            message("getting cached data")
            return(minverse)
      }
      data <- x$get()
      minverse <- solve(data, ...)
      x$setinverse(minverse)
      minverse
}
