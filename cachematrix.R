##Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than compute it repeatedly. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        matrixInverse <- NULL
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
        get <- function() x
        setMatrixInverse <- function(inverse) matrixInverse <<- inverse
        getMatrixInverse <- function() matrixInverse
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.Write a short comment describing this function

cacheSolve <- function(x, ...) {

        matrixInverse <- x$getMatrixInverse()
        if(!is.null(matrixInverse)) {
                message("getting cached data")
                return(matrixInverse)
        }
        data <- x$get()
        matrixInverse <- solve(data, ...)
        x$setMatrixInverse(matrixInverse)
        matrixInverse
}


## Testing
## x <- matrix(1:4, nrow=2, ncol=2)
## matrixInverse <- makeCacheMatrix(x)
## solve <- cacheSolve(matrixInverse)
## print (solve)
