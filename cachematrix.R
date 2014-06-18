## This function creates a special "matrix" object that can cache its inverse
## It allows users to set or get the matrix or inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv)  inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}       
## This function returns a matrix that is the inverse of matrix 'x'
## It will first attempt to get the cached inverse.
## If the inverse is not in cache, it will solve the inverse and then cache it before returning

cacheSolve<- function(x, ...) {                
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}