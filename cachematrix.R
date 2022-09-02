
##makeCacheMatrix() reurns a special object that 
## stores the and its inverse. It exploits R's scoping rules and can
## preserve state inside an R object.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

## cachSolve() returns the inverse of a matrix. 
## If such an inverse already exists, return the cached inverse. Otherwise,
## compute it, store it in the object (created by makeVector()), and return
## it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  # If the cached inverse exists, return it and the function ends
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # If the cached inverse does not exist, then compute, cache and 
  # return it.
  my.mat <- x$get()
  inverse <- solve(my.mat)
  x$setinv(inverse)
  inverse
}