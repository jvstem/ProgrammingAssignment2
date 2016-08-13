## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}




## Write a short comment describing this function
##get the matrix and calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    
    message("getting cached data")
    return(inv)
  }
  
  mtx <- x$get()
  inv <- solve(mtx, ...)
  x$setInv(inv)
  inv
}


