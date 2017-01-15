## Matrix inversion using the cache in order to save the inverse and not recalculate

##  makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        x.inv <- NULL
        set <- function(y) {
                x <<- y
                x.inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) x.inv <<- inverse
        getinverse <- function() x.inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix  
## funcion. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        x.inv <- x$getinverse()
        if(!is.null(x.inv)) {
                message("getting cached matrix")
                return(x.inv)
        }
        matriz <- x$get()
        x.inv <- solve(matriz, ...)
        x$setinverse(x.inv)
        x.inv
}
