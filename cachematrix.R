
## Matrix object that caches the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
## Set and get the matrix  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
## Set and get the inverse
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  list(set = set, get = get,
       setInverse = set_inverse,
       getInverse = get_inverse)
}

## Compute the inverse of the matrix, unless it has already been solved, then 
## return the cached inverse.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
## Return the inverse if it set.
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
## Solve the inverse of the matrix if it is not already set.
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
