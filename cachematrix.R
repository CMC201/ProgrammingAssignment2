## These functions will create a matrix object that can store
## its own inverse in its cache, and invert a matrix by first checking whether
## the inverse has already been calculated and stored in the cache defined by 
## the first function, then computing the inverse otherwise.

## This function will create a special matrix object that stores its own inverse
## in a cache for later retrieval without recomputing it.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will invert a matrix by first checking the cache to see if the inverse
## has already been computed and stored, then computing the inverse if it has not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
