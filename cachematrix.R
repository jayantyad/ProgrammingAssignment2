## The below Fnctions creates a Special Objects that stores a Matrix and caches its inverse

## The first function, makeCacheMatrix creates a special “matrix”:


makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) j <<- inverse
  getinverse <- function() j
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special “matrix”

cacheSolve <- function(x, ...) {
  j <- x$getinverse()
  if (!is.null(j)) {
    message("getting cached data")
    return(j)
  }
  data <- x$get()
  j <- solve(data, ...)
  x$setinverse(j)
  j
}
