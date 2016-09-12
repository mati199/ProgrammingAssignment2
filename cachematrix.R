## Creates the cache matrix object, returns a list of functions which will be passed to cacheSolve
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## computes inverse matrix accessing the cache object (and writing to it if empty) using list returned from makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  
  # if m is not null, return inverse and exit
  if(!is.null(m)) {
    message("already done, getting cached data")
    return(m)
  }
  
  # .. otherwise compute the inverse, set, and return
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}

# call as follows eg:
# a <- matrix(1:4,2,2)
# x <- makeCacheMatrix(a)
# cacheSolve(x)
# subsequent calls of cacheSolve(x) will display 'getting cached data'