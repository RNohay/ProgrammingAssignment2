## Filename:    cacheMatrix.R
## Author:      Rex Nohay
## Description: Because solving inverse of matrix are usually costly
##              computations, the two functions below are used together to 
##              cache the computation of matrix inverse.

## makeCacheMatrix()    creates a function object for setting and getting
##                      cached inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve       solve the inverse of a list object returned by
##                  makeCacheMatrix. If the inverse is already solved for the
##                  matrix, the cached result will be returned, else, the inverse
##                  will be solved and stored in cache.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
