## cached matrix inversion functions for 
## programming assignment 2

## creates a matrix with getters and setters
## so regetting of an inverse matrix gets
## result form cache instead of recalculating

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    get <- function() mat
    set.inverse <- function(solve) inv <<- solve
    get.inverse <- function() inv
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## cache solves the inverse of a matrix
## if already calculated returns cached
## results instead

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get.inverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set.inverse(inv)
    inv
}
