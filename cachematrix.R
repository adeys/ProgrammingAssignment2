## makeCacheMatrix creates a closure that maintains state for a matrix and its inverse.
## It returns a list of four functions:
##   - set: replaces the matrix and clears the cached inverse
##   - get: returns the current matrix
##   - setinverse: stores the computed inverse
##   - getinverse: retrieves the cached inverse (or NULL if not yet computed)
##
## The <<- operator modifies variables in the parent environment, allowing the
## closure to maintain the matrix and its inverse across multiple function calls.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv

  list(
    set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## cacheSolve computes the inverse of a cached matrix.
## It checks if the inverse has been computed before. If so, it returns the cached
## value immediately. Otherwise, it computes the inverse using solve(), stores it
## in the cache, and returns it.
##
## The ... parameter allows additional arguments to be passed to solve() if needed.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()

  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)

  inv
}
