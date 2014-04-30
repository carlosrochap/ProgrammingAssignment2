## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    #set the value of the matrix X
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function () x
    
    #set the inverse of x into the inv variable
    setInv <- function (inverse) inv <<- inverse
    getInv <- function () inv
    
    
    #Expose functions
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    #get matrix data and compute its inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    
    inv
}
