
## GitHub user:mak140430

## makeCacheMatrix:This function creates a special "matrix" object that can cache its inverse.
## this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


#### Calculate the inverse of the special "matrix" created with the above
## function by reusing cached result if it is available
## make sure that the matrix has to be invertible or it will break


cacheSolve <- function(x=inverse(), ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
