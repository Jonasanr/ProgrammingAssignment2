
## The function creates a matrix o that can cache the inverse.
makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmat <<- inverse
  getinverse <- function() invmat
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## This function computes the inverse of the matrix created by 
## makeCacheMatrix 
cacheSolve <- function(x, ...) {
  invmat <- x$getinverse()
  if (!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinverse(invmat)
  invmat
}

