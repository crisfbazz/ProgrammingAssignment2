## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## iniatialize the inverse of the matrix as NULL
    inv <- NULL
    
    ## function to set a new matrix and it's inverse as NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }    
    ## function to return the matrix
    get <- function() x
    ## funtctions to set and return the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    ## return a list containing all the values
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## analises if the inverse has already been calculated, if so return it
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## if not, calculate it and then return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    inv
}
