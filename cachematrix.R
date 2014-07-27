## makeCacheMatrix is a function which creates a special "matrix" object. 
## cacheSolve calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will be retrieved from the cache 
## and returned. 

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix created with
## makeCacheMatrix function. If available in cache, cacheSolve
## retrieves it. If not, then it computes, caches, and returns the inverse.

cacheSolve <- function(x, ...) {
    inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
    } 
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
 }