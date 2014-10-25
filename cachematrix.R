## Put comments here that give an overall description of what your
## functions do

## This function creates a square matrix. Inputs: X numeric vector; row: number of rows; col: number of columns
## Row and col should have the same value, so a square matrix is created using vector x.

makeCacheMatrix <- function(x = numeric(),row,col) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(){
      matrix(x,row,col)
  } 
  setinverse <- function(solve){
    m <<- solve
  } 
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

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
