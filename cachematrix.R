## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Caches the Matrix inverse
makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- solve(x)
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
#returns the inverse of a matrix from cache . If cached value is not available it generates the same and stores in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached inverse matrix ")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  print(inv)
}
