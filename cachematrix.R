## A pair of functions can be used to create a matrix and caches its inverse.



## Creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
           x <<- y
           s <<- NULL
    }
    get <- function() x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## Computes the inverse of the matrix object created by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    s <- x$getinv()
    if(!is.null(s)) {
    message("getting cached data")
    return(s)
    }
    invert <- x$get()
    s <- solve(invert, ...)
    x$setinv(s)
    s
}
