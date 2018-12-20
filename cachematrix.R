## These functions chche the inverse of a Matrix and store the matrix and cache its inverse. 

## This function cache the inverse of the very Matrix. 

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y){
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setInvrs <- function(inverse) invrs <<- inverse
    getInvrs <- function() invrs
    list(
        set = set, 
        get = get, 
        setInvrs = setInvrs, 
        getInvrs = getInvrs 
    )
}


## It'll calculate the inverse of the very matrx from the above function:'makeCacheMatrix'. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invrs = x$getInvrs()
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    matrx <- x$get()
    invrs <- solve(matrx, ...)
    x$setInvrs(invrs)
    invrs
}
