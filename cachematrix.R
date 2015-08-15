## This file contains two functions used to create a
## matrix with cached inverse
## makeCacheMatrix(x) create the matrix with cached inverse
## cacheSolve(x) get the cached inverse if available, otherwise
##   it compute the inverse, cache it and return it.


## makeCacheMatrix: create the matrix with cached inverse
## arguments: a 'raw' matrix
## value: list of functions: 
##      set, get: set/get the matrix
##      set.inv, get.inv: set/get the cached inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set.inv <- function(val) inv <<- val
    get.inv <- function() inv
    list(set = set, get = get,
         set.inv = set.inv,
         get.inv = get.inv)
}


## cacheSolve: get the inverse of CacheMatrix
## arguments: a CacheMatrix made by makeCacheMatrix,
##      and other parameters for the inverse computation
## value: the inverse matrix
cacheSolve <- function(x, ...) {
    inv <- x$get.inv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set.inv(inv)
    return(inv)
}
