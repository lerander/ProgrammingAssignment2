## Two functions that work together to facilitate caching of the
## inverse of a matrix. Use makeCacheMatrix to create the cacheable
## matrix, and cacheSolve to get it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Creates a matrix with functions for setting and getting it's contents,
        ## and also for getting and setting it's inverse.
        inv <- NULL
        ## The '<<-'-operator uses the parent environment to persist values 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverted) inv <<- inverted
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Returns the inverse of matrix x. (x must be created
        ## by makeCacheMatrix.) If the inverse is calculated previously,
        ## and x is unaltered since then, a cached inverted matrix
        ## will be returned.
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Did not find inverted matrix; therefore calculating it.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
