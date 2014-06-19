## Creates a special matrix object that can cache its inverse

## The first function 'makeCacheMatrix' creates a special sort of vector that is
## really a list containing functions to set and then get the value of the vector,
## then set and get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set= set, get= get,
         setinverse = setinverse
         getinverse = getinverse)
}


## Return a matrix 'cacheSolve' that is the inverse of 'x' that
## computes the inverse.  If the matrix has already been calculated
## (and the matrix has not changed), then the function 'cacheSolve' will retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (lis.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
