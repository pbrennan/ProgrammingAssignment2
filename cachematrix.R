## Program creates a special matrix that can have its square matrix calculated, and
## if already calculated, the cached result will be returned

## Creates a special matrix that caches its inverse
## This funciton assumes that the input is a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    
    list(
        set = set, 
        get = get, 
        setInverse = setInverse, 
        getInverse = getInverse
    )
}

## Calculates the inverse of the special matrix returned by makeCacheMatrix
## If the result is already calculated, then return the cache result instead

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setInverse(i)
    i
}
