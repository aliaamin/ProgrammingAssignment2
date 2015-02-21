## Put comments here that give an overall description of what your functions do
# 
## Write a short comment describing this function

## This function that cache the inverse of matrix that is specified
## Returns a list of function: set, get, setinverse, getinverse

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


## This function returns a matrix that is inverse of the "X"
## If the inverse matrix has been stored earlier in the cache then return that value, otherwise calculate inverse of that matrix with the function "solve"

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
