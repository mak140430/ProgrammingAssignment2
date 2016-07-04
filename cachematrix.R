## Week3 assignment: makeCacheMatrix:This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
##already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## GitHub user:mak140430

## this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- inverse(data, ...)
        x$setinverse(inv)
        inv
}
