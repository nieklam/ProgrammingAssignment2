#Since matrix inversion is usually a costly computation and there may be some 
#benefit to cache the inverse of a matrix (rather than compute it repeatedly).
#To do so 2 functions are written: makeCacheMatrix and cacheSolve.


## The function makeCacheMatrix accepts a matrix as parameter, and it stores 
## a list of 4 functions:
## set() : sets the matrix to be inverted
## get() : gets the matrix to be inverted
## setInverted() : sets the computed inverse matrix  
## getInverted() : gets the computed inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverted <- function(solve) m <<- solve
        getInverted <- function() m
        list(set = set, get = get,
             setInverted = setInverted,
             getInverted = getInverted)
}



## The function cacheSolve calculates the inversed matrix. However, it first 
## checks to see if the inversed matrix has already been calculated. If so, 
## it gets the inversed matrix from the cache and skips the computation. 
## Otherwise, it calculates the  inversed matrix of the data (that is a matrix) 
## and sets the calculated inversed matrix in the cache via the setInverted 
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverted()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverted(m)
        m
}
