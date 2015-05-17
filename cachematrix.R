# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. 
#Here the functions that cache the inverse of a matrix.

# makeCacheMatrix creates a list containing functions to
# 1._Set the value of the matrix
# 2._Get the value of the matrix
# 3._Set the value of inverse of the matrix
# 4._Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        mx <- NULL
        set <- function(y) {
                x <<- y
                mx <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) mx <<- inverse
        getInverse <- function() mx
        list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        mx <- x$getInverse()
        if(!is.null(mx)) {
                message("getting cached data")
                return(mx)
        }
        data <- x$get()
        mx <- solve(data, ...)
        x$setInverse(mx)
        mx
}
