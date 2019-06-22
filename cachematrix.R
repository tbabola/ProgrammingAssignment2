## These functions help prevent costly recalculation of inverses of large matrices by storing the result
## in a cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solved) m <<- solved
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## This function computers inverse of the special "matrix" returned by 
## makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
