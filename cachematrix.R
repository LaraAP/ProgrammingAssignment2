#makeCacheMatrix creates a list of functions
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  } #create the matrix
  get <- function() x #get matrix value
  setinv <- function(inverse) i <<- inverse  #invert  matrix
  getinv <- function() i #get invert matrix from cache
  list(set = set, get = get, setinv = setinv, getinv = getinv) #list of created functions
}

## cacheSolve - calculate inverse matrix
cacheSolve <- function(x, ...) {
  i <- x$getinv() #try to get inv matrix stored in cache
  if (!is.null(i)) {
    message("cached data")
    return(i)
  } # return inverted matrix from cache. if the matrix does not exists -> create a matrix
  ma <- x$get() #create matrix if it does not exist
  i <- solve(ma, ...)
  x$setinv(i)
  i
}


#### TESTING ####
M1 <- matrix(c(1,2,3,4),2,2)
M2 <- makeCacheMatrix(M1)
cacheSolve(M2)
