## Cache the inverse of a matrix

## This function creates a special "matrix" object, which is really a list containing a function to
##     1. set the matrix
##     2. get the matrix
##     3. set the inverse of the matrix
##     4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invr <<- inverse
        getInverse <- function() invr
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invr <- x$getInverse()
        if(!is.null(invr)) {
                message("getting cached data")
                return(invr)
        }
        data <- x$get()
        invr <- solve(data, ...)
        x$setInverse(invr)
}
