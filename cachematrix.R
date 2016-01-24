## The source code below defines two functions: makeCacheMatrix() and cacheSolve()
##
## function makeCacheMatrix() builds a list to wrap a matrix and its inverse 
##
## function cacheSolve() computes the inverse of a matrix if not previously cached
## and then stores it.

## Builds a "matrix container", i.e. a list exposing functions to set and get
## a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) invx <<- inverse
    getinv <- function() invx

    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns the inverse of the matrix wrapped by 'x'.
## If 'x' already stores the inverse matrix, the cached value is returned
## otherwise the inverse of 'x' is computed, cached and returned.
## Please note the function intentionally has a single return point at the end.
##
cacheSolve <- function(x, ...) {
    invx <- x$getinv()
    if(!is.null(invx)) {
        # The message gives evidence of the cache hit
        message("getting cached data")
    }
    else {
        # Inverting the matrix and storing the value in 'x'
        invx <- solve(x$get(), ...)
        x$setinv(invx)
    }
    # Returning the result
    invx
}
