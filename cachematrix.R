## makeCacheMatrix makes matrix based object that may cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mm <- NULL
  set <- function(y) {
    x <<- y
    mm <<- NULL
  }
  ## get the matrix value.
  get <- function() x
  ## set the matrix inverse.
  setinv <- function(i) mm <<- i
  getinv <- function() mm
  ## get the the matrix inverse.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve calculate invers for matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Check if matrix already has inverse
  mm <- x$getinv()
  if(!is.null(mm)) {
    message("getting cached data")
    return(mm)
  }
  data <- x$get()
  mm <- solve(data, ...)
  x$setinv(mm)
  mm
}
