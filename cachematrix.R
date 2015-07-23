## Code to cache the inverse of a matrix 


## makeCacheMatrix() will create a list of functions that will set the matrix and cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
                mat <- NULL
                set <- function(y) {
                        x <<- y
                        mat <<- NULL
                }
                get <- function() x
                setinverse <- function(matrix) mat <<- matrix
                getinverse <- function() mat
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## cacheSolve() will calculate the inverse matrix and use the function list in 
## makeCacheMatrix to cache it

cacheSolve <- function(x, ...) {
        mat <- x$getinverse()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setinverse(mat)
        mat
}
