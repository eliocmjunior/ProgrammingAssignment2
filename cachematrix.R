## This is a set of functions that could improve time processing
## The first function stores the inverse of a matrix in cache and the second solves it if it was not been processed

## Put matrix on cache memory: returns a list with variables that can be accessed in global enviroment
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

## Verify cached matrix and computes.
## Note that this functions must to receive the previous to get variables (x$get_inv and x$get)

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
