## Functions to manipulate the invertible matrix with cached inverse.

## Creates special type of matrix that
## may contain cached value of its inverse and
## has 4 methods: set, get, getinverse, setinverse.
## m: matrix, assumed to be square and invertible
makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Finds the inverse of matrix.
## m: matrix created by makeCacheMatrix()
cacheSolve <- function(m, ...) {
    i <- m$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- m$get()
    i <- solve(data, ...)
    m$setinverse(i)
    i
}
