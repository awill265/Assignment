## this function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        ## sets the matrix
        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        ## gets the matrix 
        get <- function() m
        
        ## sets the inverse of the matrix
        setinverse <- function(inverse) i <<- inverse
        
        ## gets the inverse of the matrix
        getinverse <- function() i
        
        ## lists the methods used
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## calculates the inverse of the special matrix created with the above function
## checks to see if inverse has already been calculated
## if so, gets inverse from the cache and skips the calculation
cacheSolve <- function(x, ...) {
        
        ## gets the inverse of x matrix
        i <- x$getinverse()
        
        ## if inverse is already computed, returns that matrix
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## gets the matrix from the data
        data <- x$get()
        
        ## computes the inverse of the matrix
        i <- solve(data, ...)
        
        ##sets the inverse of the matrix to i 
        x$setinverse(i)
        i
}