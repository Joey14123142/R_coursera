## Caching the inverse of a Matrix

## Create another matrix that can cache its reverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinver <- function(t) inver <<- t
  getinver <- function() inver
  list(set = set, get = get, setinver = setinver, getinver = getinver)
}

## Return the inversed matrix either from cache or by computing

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getinver()
  if(!is.null(inver)){
    message("getting cached matrix")
    return(inver)
  }
  mat <- x$get()
  inver <- t(mat, ...)
  x$setinver(inver)
  inver
}