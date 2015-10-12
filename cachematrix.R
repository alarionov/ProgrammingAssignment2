makeCacheMatrix <- function (x = matrix()) {
  inv <- NULL # inverse matrix
  
  # method to set matrix
  set <- function (y) { 
    x   <<- y
    # we need to reset the inverse matrix
    # when we change the matrix
    inv <<- NULL
  }
  
  # method to get the matrix
  get <- function() x
  
  #method to set inverse
  setinv <- function(y) inv <<- y
  
  # method to get the inverse matrix
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function (x, ...) {
  # trying to get cached value
  inv =  x$getinv()
  
  # if there is no cached value
  if (is.null(inv)) {
    # compute it
    message("getting cached data")
    inv = solve(x$get(), ...)
    
    # and save
    x$setinv(inv)
  }
  
  # return cached value
  inv
}