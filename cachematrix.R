## makeCacheMatrix creates an object with a square matrix and 
##   a cached version of its inverse.  cacheSolve will return 
##   the inverse of the matrix by either retrieving the cached inverse
##   or creating it if it does not already exist

## makeCacheMatrix:  Create a special matrix that can cache it's own inverse
##   First, initialize the cached inversion of the matrix (m) to NULL
##   set:  set the matrix (x) to the input variable, and initialize the 
##      the cached version (m) of the inverse to NULL
##   get:  return the matrix
##   setinverse:  create the cached inverse (m)
##   getinverse:  return the cached inverse (m), NULL is returned if
##      the cached version has not been created

makeCacheMatrix <- function(x = matrix()) {
	
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve:  Return a matrix that is the inverse of 'x'
## If the inverse has already been computed, return
## the cached version.
## If it has not been computed (i.e., the matrix is null),
## compute the inverse using solve() and set the cached
## inverse accordingly.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
