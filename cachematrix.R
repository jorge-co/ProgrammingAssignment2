## The following functions keep a function structure in the cache to check if the 
## inverse of a specific matrix has already been calculated.

## Caches an matrix with functions that allow you to get data from the array
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}
## Searches if the inverse has already been calculated for the matrix, and if not,  it does 
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
