## Put comments here that give an overall description of what your
## functions do

## Stores the cached value of inverse of matrix x
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    setinv <- function(invMat) {
        inv <<- invMat
    }
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## Compute the inverse of a matrix if the value was
## already calculated get it from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting the cached value")
        return (inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
