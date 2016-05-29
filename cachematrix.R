## The functions in this file can be used to create a matrix that 
## can cache the inverse of itself, so that if the inverse is needed
## again, and the matrix has not changed, the cached information
## can be used instead of being recalculated

## Create a special matrix that can store the inverse of itself

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    setinverse <- function(inver) inv <<- inver
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function will calculate the inverse of a square, invertible 
## cacheable matrix, unless the cacheable matrix already has the 
## inverse stored, in which case the stored, or "cached" value will 
## be returned.  
## Note that this function *assumes* the cached matrix is invertible!!!

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
