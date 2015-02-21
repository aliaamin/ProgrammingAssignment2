## Put comments here that give an overall description of what your functions do
# 
## Write a short comment describing this function

## This function that cache the matrix that is specified
# returns a list of function: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
        imx <- NULL                         #initial variable set to NULL
        set <- function(y) {                
                x <<- y
                imx <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) imx <<- solve
        getinverse <- function() imx
        list(set = set, get = get,            
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## this function returns a matrix that is inverse of the "X"
#if it has been specified earlier in cache then return that value from cache, otherwise calculate inverse of that matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        imx <- x$getinverse()
        if(!is.null(imx)) {                              
                message("getting cached matrix")
                return(imx)
        }
        data <- x$get()
        imx <- solve(data, ...)          
        x$setinverse(imx)
        imx

}
