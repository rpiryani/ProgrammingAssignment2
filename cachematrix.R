################################################################################
## This script will create two function 
## makeCacheMatrix:- It  creates a special "matrix" object which can cache its 
##                  inverse.
## cacheSolve:- It computes the inverse of special "matrix" which is returned by
##              makeCacheMatrix funcion. It will check, if inverse has already  
##              been calculated(if the matrix is not changed), then cacheSolve 
##              function will retrive inverse data from cache. Otherwise it will
##              compute inverse.
################################################################################
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(matrix) {
        x <<- matrix
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
