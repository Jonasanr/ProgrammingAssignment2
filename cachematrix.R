
## The function creates a matrix o that can cache the inverse.
makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinversemat <- function(inverse) invmat <<- inverse
  getinversemat <- function() invmat
  list(set = set,
       get = get,
       setinversemat = setinversemat,
       getinversemat = getinversemat)
}
## This function computes the inverse of the matrix created by 
## makeCacheMatrix 
cacheSolve <- function(x, ...) {
  invmat <- x$getinversemat()
  if (!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  mat <- x$get()
  invmat <- solve(mat, ...)
  x$setinversemat(invmat)
  invmat
}
