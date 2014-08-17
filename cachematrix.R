## cached matrix inversion functions for 
## programming assignment 2

## creates a matrix with getters and setters
## so regetting of an inverse matrix gets
## result form cache instead of recalculating

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(setmat) {
        mat <<- setmat
        inv <<- NULL
    }
    get <- function() mat
    set.inverse <- function(setinv) inv <<- setinv
    get.inverse <- function() inv
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## cache solves the inverse of a matrix
## if already calculated returns cached
## results instead

cacheSolve <- function(cached.mat, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- cached.mat$get.inverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- cached.mat$get()
    inv <- solve(data, ...)
    cached.mat$set.inverse(inv)
    inv
}
