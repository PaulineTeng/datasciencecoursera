## This function computes the inverse of the matrix.

## makeCacheMatrix comes with 4 functions
## set is to save the input matrix in the cache
## get is to retrieve the input matrix in the cache
## setinverse is to save the calcualted matrix in the cache
## getinverse is to retrieve the calculated matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix
## and output it in matrix format

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting inverse data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
