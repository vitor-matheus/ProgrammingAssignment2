## Put comments here that give an overall description of what your
## functions do:
## Its jobs is to cache the inverse of a matrix.

## Write a short comment describing this function:
## This function creates a special "matrix" object. It can cache its inverse
## for the input.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" above.
## If its already calculated, the "cacheSolve" should retrieve the cache's
##inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
