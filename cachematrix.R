## Put comments here that give an overall description of what your
## functions do

## Put matrix on cache memory

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
      get <- function() x
      set_inv <- function(solve) m <<- solve
      get_inv <- function() m
      list(set = set, get = get,
           set_inv = set_inv,
           get_inv = get_inv)
      }

## Verify cached matrix and computes

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inv(m)
        m
}
