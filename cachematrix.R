## This file is my response to Week 3 
## Programming Assignment 2
## MainStreetQuant

## This function is intended to cache the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Start by ensuring there is nothing residual already saved.
  
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get, then store, both the matrix and its inverse
  
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## This function is intended to solve the inverted matrix.

cacheSolve <- function(x, ...) {
  
  ## Check whether we already have a freshly inverted matrix ready.
  
  inv = x$getinv()
  
  ## If we do not yet have the inverted matrix, retrieve it from storage.
  
  if (!is.null(inv)){
    message("fetching cached matrix...")
    return(inv)
  }
  
  ## Otherwise, solve what we already have.
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}


