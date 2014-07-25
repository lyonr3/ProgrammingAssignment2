## Functions take a matrix as input and generate its inverse
##    the inverse is cached and pulled from cache on subsequent runs

## This sets up the instance

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setimatrix <- function(solve) m <<- solve
  getimatrix <- function() m
  list(set = set, get = get,
       setimatrix = setimatrix,
       getimatrix = getimatrix)
}


## This function generates or restores the cached inverse

cachesolve <- function(x, ...) {
   m <- x$getimatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setimatrix(m)
  m
}
