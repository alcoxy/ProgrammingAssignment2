## Creates a list of functions in order to calcultate the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(m_inverse) inv <<- m_inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Get the matrix inverse, or calculate it

cacheSolve <- function(x, ...) {
   inv <- x$getinv()
   if (!is.null(inv)) {
     message("getting cached data")
     return(inv)
   }
   data <- x$get()
   inv <- solve(data, ...)
   x$setinv(inv)
   inv
  ## Return a matrix that is the inverse of 'x'
}
