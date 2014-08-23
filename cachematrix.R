################################################################################
## This script will create two function 
## 1. makeCacheMatrix:- 
## 2. cacheSolve:- 
################################################################################

## makeCacheMatrix:- It  creates a special "matrix" object which can cache its 
##                  inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## function for set the matrix
    set <- function(matrix) {
        x <<- matrix
        i <<- NULL
    }
    ## function for get the matrix
    get <- function() x
    ## set the inverse of matrix
    setinverse <- function(inverse) i <<- inverse
    ## get the inverse of matrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve:- It computes the inverse of special "matrix" which is returned by
##              makeCacheMatrix funcion. It will check, if inverse has already  
##              been calculated(if the matrix is not changed), then cacheSolve 
##              function will retrive inverse data from cache. Otherwise it will
##              compute inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## getting the matrix data
    data <- x$get()
    ## calling solve function to find the inverse of matrix
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
