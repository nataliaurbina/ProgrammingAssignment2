## Inverse matrix calculations can take up a lot of time. These functions allow 
## the caching of the inverse of a matrix for latter use, avoiding its 
## recalculation.

# This matrix has an inverse and can be used to test out these functions:
# m <- matrix(1:4, nrow = 2, ncol = 2)     

## makeCacheMatrix creates a matrix object, which can be cached. It contains 4 
## functions:
## set() overwrites the matrix object
## get() returns the contents of the matrix object
## setInverse() sets the inverse matrix
## getInverse() retrieves the cached inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve returns the inverse matrix in one of two ways:
## 1) It searches for a cached inverse matrix, via the getInverse() function.
##    If a matrix is found, it notifies the user via a message and then returns
##    the inverse matrix.
## 2) If there is no cached matrix, it calculates the inverse of the current 
##    matrix object and sets it via the setInverse() function, then it is 
##    returned.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message ("getting cached matrix")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv 
}
