## These functions support caching of time-consuming matrix inversions
## by introducing a function 'makeCacheMatrix' creating a vector
## of functions used to store a cached result besides the actual
## input data. This vector of functions is used by the 'cacheSolve'
## function to check whether a cached result is available. If yes,
## the cached result is returned. If not, the input is used to
## calculate the inverse matrix and the result is cached and returned.

## Create a vector of functions for getting and setting the input matrix and the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                ## reset if new value is set
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Get the inverse matrix either from the cache if available or by calculating (and caching it)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## getting non-cached data
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
