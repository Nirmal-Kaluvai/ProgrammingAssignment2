## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special “matrix" which is really used to set and get the values of the matrix and its inverse. 



makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i        ## Return a matrix that is the inverse of 'x'
}
T <- matrix(c(1,2,3,4),2,2)
T1 <- makeCacheMatrix(B)
cacheSolve(T1)#inverse returned after computation
cacheSolve(T1)#inverse returned from cache

