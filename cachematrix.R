## These functions calculates the invaerse of a square matrix in a less time-consuming computational method b y making use of the method 
## of cache and making use of different environments to store values

## This function creates a list of functions: set, get, setinv, getinv. These define functions in an environment that will
## be used in the next function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function takes a square matrix and calculates the inverse of the matrix. It makes use of the previously defined functions 
## and check other environments for already calculated results.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
