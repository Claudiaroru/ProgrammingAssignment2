## Put comments here that give an overall description of what your
## functions do

## The first function (makeCacheMatrix) creates a list that contains the functions to define the values in the matrix and to get then to set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(inv) m <<- inv
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## The second function (cacheSolve), in the first place, checks if the solution to the inverse matrix is already in cache and, if it is, it returns the solution..
## In the second place, if the solution was not found in the cache, it calculates the inverse of the matrix (by the solve function).

cacheSolve <- function(x, ...) {
  m <- x$getinvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvmatrix(m)
  m
## Return a matrix that is the inverse of 'x'
}
