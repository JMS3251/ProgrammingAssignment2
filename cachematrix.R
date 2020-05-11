## cachematrix.R contains two functions that are used to create and 
## maintain a matrix as an R object.

## makeCacheMatrix creates an R object that stores a matrix 
## and its inverse.
##
makeCacheMatrix <- function(x = matrix()){
    minv <- NULL
    set <- function(y){
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) minv <<- solve
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve requires a matrix object of the type that is returned
## by makeCacheMatrix.  It will retrieve the inverse of the matrix
## if it is cached; otherwise, it will calculate, cache, and return
## the inverse matrix.
##
cacheSolve <- function(x, ...){
    
    minv <- x$getinv()
    if(!is.null(minv)){
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinv(minv)
    minv
}