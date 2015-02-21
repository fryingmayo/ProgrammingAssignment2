## makeCacheMatrix and cacheSolve
##
## makeCacheMatrix:
## Description:
## Sets up a closure containing four functions.
##
##    set - sets closure with new matrix
##    get - returns matrix from closure
## setinv - meant for use by cacheSolve
##          sets the inverse of the stored matrix
## getinv - returns the inverse of the matrix
##
## Use:
## Create a makeCacheMatrix closure:
## xx <- makeCacheMatrix(x)
## where xx is an instantiated makeCacheMatrix closure
##        x is an invertible matrix
##
## Set a new invertible matrix into an existing closure:
## xx$set(x)
## Where x is an invertible matrix
##
## Get the stored matrix value:
## xx$get()
##
## cacheSolve:
## Description:
## Pulls the inverse matrix value from a makeCacheMatrix
## closure and returns its value.  If it doesn't exist (NULL),
## then compute the inverse, cache it in the makeCacheMatrix 
## closure, then return the result.
##
## Use:
## cacheSolve(xx)
## where xx is an instantiated makeCacheMatrix closure
##

## makeCacheMatrix - sets up a closure to store a matrix
## and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get=get,
       setinv = setinv, getinv = getinv)
}


## cacheSolve - Pulls the cached inverse value from a 
## makeCacheMatrix closure.  If it does not exist, then 
## cacheSolve will compute the inverse, store the result in the
## makeCacheMatrix closure, and return the result.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    ## Return a cached matrix that is the inverse of 'x'
    print("Retreiving cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  inv

}
