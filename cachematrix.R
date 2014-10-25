## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## This function will create a matrix object that can cache its inverse.  
## Also, this function has a list of functions which set and get cached 
## matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## set funcion to set the matrix 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get function to get the matrix
        get <- function() x
        
        ## set inverse matrix function
        setMatrix <- function(solve) m <<- solve
        
        ## get inverse matrix function
        getMatrix <- function() m
        
        ## List of functions
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## The following function calculates the inverse of the given matrix.  
## If the inverse of the matrix is stored in the cache, then it will 
## skip the calculation and return the inverse matrix. 
## Otherwise this function will calculate the inverse of the matrix and 
## store the inverse matrix in the cache via the setMatrix function.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## get inverse matrix
        m <- x$getMatrix()
        
        ## if inverse matrix is in the cache (m is not null) then return
        ## the inverse matrix from the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## get the matrix
        data <- x$get()
        
        ## calculate the inverse matrix
        m <- solve(data, ...)
        
        ## set the inverse matrix to cache
        x$setMatrix(m)
        
        ## return inverse matrix
        m
}
