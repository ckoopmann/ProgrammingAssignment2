## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix as input and returns a list of functions the "CacheMatrix"

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y){
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function takes a "CacheMatrix" created by the previous function as input and returns it's inverse as a normal matrix and also caches the solution. Before calculating the inverse it checks if the inverse is already cached. If so it just returns that solution.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i## Return a matrix that is the inverse of 'x'
}
