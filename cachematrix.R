## Put comments here that give an overall description of what your
## functions do

## This function creates a special "array" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        matrixinv <- NULL
        set <- function(y) {
                x <<- y
                matrixinv <<- NULL
            }
        get <- function() x
        setInverse <- function(inverse) matrixinv <<- inverse
        getInverse <- function() matrixinv
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## This function calculates the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been computed (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixinv <- x$getInverse()
        ## Validate matrixinv has data
        if (!is.null( matrixinv)) {
                return(matrixinv)
        }
        data <- x$get()
         matrixinv <- solve(data, ...)
        x$setInverse(matrixinv)
        matrixinv
}
