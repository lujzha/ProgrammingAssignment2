## The following 2 functions will allow the user to
## ... cache the inverse of an invertible function

## makeCacheMatrix generate a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      sol <- NULL
      set <- function(y){
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setsol <- function(inverse) sol<<- inverse
      getsol <- function() sol
      list(set = set, get = get, setsol = setsol, getsol = getsol)
}


## cacheSolve finds the inverse of matrix associated with a makeCacheMatrix object
## and returns its inverse, or if the inverse is cached, returns the cached inverse
cacheSolve <- function(x, ...) {
      sol <- x$getsol()
      if(!is.null(sol)) {
            message("getting cached data")
            return(sol)
      }
      data <- x$get()
      sol <- solve(data, ...)
      x$setsol(sol)
      sol
}

