## Calculates the inverse of a matrix and saves it.  
## if the inverse of the same matrix is needed again later, 
## a cached copy of the answer will be returned instead of 
## doing the calculation again.

## creates a special object that can store a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(mean) m <<- mean
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## finds the inverse of the matrix by finding a cached value 
## if available.  If not, it will be solved.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
