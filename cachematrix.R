## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list of functions of a matrix with value x:
## set - read in a matrix and set x to this value;
## get - get the value of x;
## setinv - read in a matrix and set inverse of x to this value;
## getinv - get the value of inverse of matrix x.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## input argument is the return value of makeCacheMatrix
## return the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    ## if inv is not null
    ## it means the inverse has been computed and cached before
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## if inv is null
    ## get the value of the matrix, compute the inverse
    data <- x$get()
    inv <- solve(data, ...)
    ## cache the value of the inverse
    x$setinv(inv)
    inv
}
