## These functions cache the inverse of a matrix.
## If inverse of Matrix is already calculated and Matrix is not changed it will retreave the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by the above function.
## If inverse of Matrix is already calculated and Matrix is not changed it will retreave the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  i <- data %*% solve(data, ...)
  x$setinverse(i)
  i
          
}
