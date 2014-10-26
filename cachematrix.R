## The functions below are for computing and caching the inverse of a matrix

## Function name: makeCacheMatrix
## Description: 
##      Function to create special matrix object that caches 
##              the inverse of the input provided.
## Arguments:
## x    Matrix object. Assumption is that the input supplied is always
##              invertible.
## Return: 
##      Returns a list containing following functions: -
##      1.  set():set the value of the matrix
##      2.  get(): get the value of the matrix
##      3.  setinverse(): set the value of the inverse
##      4.  getinverse(): get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the special matrix variable that caches the inverse
        i <- NULL
        
        ## Function to set the value of matrix variable x and 
        ##      re-initialize the special matrix variable i.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        ## Function to return the value of the matrix
        get <- function() {
                x
        }
        
        ## Function to cache the inverse of matrix by setting the value
        ##      to special matrix variable.
        setinverse <- function(inverse) {
                i <<- inverse
        }
        
        ## Function to return the cached special matrix variable 
        getinverse <- function() {
                i
        }

        ## List of all the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## Function name: cacheSolve
## Description: 
##      Function to return the inverse matrix for the given input matrix.
##              If the inverse is already calculated and the matrix has
##              not changed, then it returns the inverse from the cache.
## Arguments: 
## x    Matrix object. Assumption is that the input supplied is always
##              invertible.
## ...  Further arguments passed to or from other methods.
## Return: 
##      Return a matrix object that is the inverse of the input matrix.
cacheSolve <- function(x, ...) {
        
        ## Get the value of inverse from the cache object
        i <- x$getinverse()

        ## If the value is defined then return from cache and exit the function
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## Get the matrix object
        data <- x$get()
        ## Inverse the matrix object
        i <- solve(data, ...)
        ## Set the inverse value to the special cache object
        x$setinverse(i)
        i
}
