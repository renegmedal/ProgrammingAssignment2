## These functions cache the inverse of a given matrix. Matrix inversion
## could be a costly processing specifically if the matrix is huge and
## being used repeatedly. These functions would cache the resulting
## inversion of matrix thus the computation can only done once

## makeCacheMatrix maintains a list of functions to process, store and
## access the inversion of matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- x        
        
        inverse <<- NULL  
        
        get <- function() m
        getInverse <- function() inverse 
        
        set <- function(matrix)  {        
                m <<- matrix
                inverse <<- NULL
        }
        
        setInverse <- function(matrix) {
                inverse <<- solve(matrix) 
        }
        
        list(get = get,
             set = set,
             getInverse = getInverse,
             setInverse = setInverse)   
}


## cacheSolve function gets the inverse of parameter matrix.
## If the inverse of matrix is already cached, then it will 
## return the cached inversed matrix. If not, it computes 
## the inversion of matrix, store in a cached variable and
## return the inversed matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
        if (!is.null(m)) {
                message("getting cached inversed data")
                return(m)
        }
        data <- x$get()
        x$setInverse(data)
        x$getInverse()
}
