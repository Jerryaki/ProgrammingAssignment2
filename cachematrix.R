## This function returns a special object that is a list with four functions:
## set: sets the input matrix and resets any cached matrix
## get: retrieves the input matrix
## setmatrix: stores a matrix in the cache
## getmatrix: retrieves cached matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x

## setmatrix doesn't actually solve for the inverse, it will store a matrix in the cache when
## the solveCache function calculates the inverse and calls setmatrix.    
    
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## This function takes the object from the makeCacheMatrix function and first checks if there is
## an inverse matrix stored in the cache; if so, it returns it. Otherwise, it calls the input
## matrix using the get function, calculates the inverse using solve, stores it to the cache
## using the setmatrix function, and then returns the inverse.

cacheSolve <- function(x = matrix, ...) { 
        m <- x$getmatrix()
        if(!is.null(m)) {
            message("Getting cached data")
            return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}