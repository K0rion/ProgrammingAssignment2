## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. The following functions are used
## to create an object, which stores a matrix and chaches its inverse.

## The makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      minv <- NULL
      set <- function(y) {
            x <<- y
            minv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) minv <<- inverse
      getinverse <- function() minv
      list(set = set, 
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve will retrieve the inverse from the cache. The function 
## assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
      minv <- x$getinverse()
      if(!is.null(minv)) {
            message("getting cached data")
            return(minv)
      }
      matrix <- x$get()
      minv <- solve(matrix, ...)
      x$setinverse(minv)
      minv
}
