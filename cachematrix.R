## a pair of functions that cache the inverse of a matrix

## create a matrix object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}

## compute the inverse of the matrix object created above,
## should retrieve from the cache if been previously computed,
## assume the inverse exists
cacheSolve <- function(x) {
        inv = x$get_inv()
        if(!is.null(inv)) {
                message("getting cashed data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$set_inv(inv)
        inv
}