##These two functions calculate the inverse of a matrix, 
## and allow obtain it from cache, if the matrix has been previously calculated


##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get = function() x
  setinv = function(inverse) i <<- inverse 
  getinv = function() i
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Computes the inverse, or retrieves form cache if previously calculated

cacheSolve <- function(x, ...) {
  i = x$getinv()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  mat.data = x$get()
  i= solve(mat.data, ...)
  x$setinv(i)
  return(i)
}
