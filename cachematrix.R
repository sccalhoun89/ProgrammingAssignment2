## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinvrs <- function(solve) invrs <<- solve
    getinvrs <- function() invrs
    list(set = set, get = get,
         setinvrs = setinvrs,
         getinvrs = getinvrs)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invrs <- x$getinvrs()
    if(!is.null(invrs)) {
        message("getting cached data.")
        return(invrs)
    }
    data <- x$get()
    invrs <- solve(data)
    x$setinvrs(invrs)
    invrs
}
