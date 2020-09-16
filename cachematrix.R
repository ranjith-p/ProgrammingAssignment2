## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix" to fill it later.

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse<- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##This function looks whether a calculation has already been made in the cache, if so, it retrieves it, if not, it calculates it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
