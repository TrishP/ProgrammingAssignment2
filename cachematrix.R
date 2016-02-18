## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of functions which
##     get the original matrix
##     set the original matrix
##     get the inverse of the matrix
##     set the inverse of the matrix
## makeCacheMatrix takes in the original matrix and stores it

## Usage example:
##   w <- matrix(1:4, ncol=2)
##   m <- makeCacheMatrix(w)
##   m$get()
##   cacheSolve(m)
##   m$getinvrs()

makeCacheMatrix <- function(x = matrix()) {
     invrs <- NULL
     set <- function(y) {
          x <<- y
          invrs <<- NULL
     }
     get <- function() x
     setinvrs <- function(minvrs) invrs <<- minvrs
     getinvrs <- function() invrs
     list(set = set, get = get,
          setinvrs = setinvrs,
          getinvrs = getinvrs)
     
}     


## cacheSolve either retrieves the matrix inverse from cache 
##      or calculates it and stores it in the cache  
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
     invrs <- x$getinvrs()
     if(!is.null(invrs)) {
          message("getting cached data")
          return(invrs)
     }
     data <- x$get()
     invrs <- solve(data, ...)
     x$setinvrs(invrs)
     invrs
}

