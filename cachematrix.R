
## This function takes a matrix, solves for the matrix inverse and stores in cache. 
## Note: x defaults to a uniform square matrix for example purposes unless x is set otherwise.

makeCacheMatrix <- function(x = matrix(runif(4),nrow=2)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve takes the makeCacheMatrix list and returns the inverse matrix. If the inverse is null, 
## then takes the matrix data and solves for the inverse. Then returns the results.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
