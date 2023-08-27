
## The following function is a set of two functions that cache the inverse of a matrix.
## Both functions are expalined bellow.


## The 'makeCacheMatrix' sets up a tool for matrices,storing and retrieving their inverses.

makeCacheMatrix <- function(x = matrix()) {  
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    getInverse <- function() {
        inv
    }
    calculateInverse <- function() {
        if (!is.null(x)) {
            inv <<- solve(x)
        } else {
            message("Matrix is not set. Use set() to set the matrix.")
        }
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse, calculateInverse = calculateInverse)
}

## The 'cacheSolve' function complements the 'makeCacheMatrix', quickly getting the inverse.
##It calculates and caches the inverse if not available


cacheSolve <- function(x, ...){
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("Retrieving cached Data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}

getwd()
