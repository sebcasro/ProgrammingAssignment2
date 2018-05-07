##The objetive of this work is write two functions, "makeCacheMatrix" and "cachesolve"

## makeCacheMatrix is a function witch creates a "matrix invers" of a inertible matrix
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
    set <- function(y) {
        x <- y
        inv <- NULL
      }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}	


## "cacheSolve" is a function which computes the inverse of the special "matrix" created by "makeCacheMatrix" above.



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'	         ## Return a matrix that is the inverse of 'x'
   inv <- x$getinv()
  if(!is.null(inv)) {
        message("getting cached result")
        return(inv)
      }
   data <- x$get()
   inv <- solve(data, ...)
   x$setinv(inv)
    inv
}	
