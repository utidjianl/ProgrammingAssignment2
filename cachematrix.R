## The goal of these 2 functions is to perform matrix inversion and save memory for checking
## to see if the inverse has already been calculated and is stored in the cache.

## The makeCacheMatrix function creates a special "matrix" object that is a
## list containing functions to:
## (1) set the value of the matrix,
## (2) get the value of the matrix,
## (3) set the value of the matrix's inverse, and
## (4) get the value of the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function (y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv_matrix <<- inverse
    getinverse <- function() inv_matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has not been calculated yet, it computes and returns the inverse.
## If the inverse has already been calculated, it retrieves the inverse from the cache
## and notes that cached data is being returned.
cacheSolve <- function(x, ...) {
    inv_matrix <- x$getinverse()
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    data <- x$get()
    inv_matrix <- solve(data)
    x$setinverse(inv_matrix)
    inv_matrix     
}
