## Following functions are used to compute, store (cache) and retrive
## inverse of a matrix.

## Creates a special object based on the specified matrix. It basically
## returns list of functions that are used to manipulate the matrix
## (set, get, setinv, getinv).

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns an inverse of the matrix. If the value was not cached before,
## it is computed, stored (cached) for later use and then returned.

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse
}
