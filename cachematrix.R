##This program creates two functions:  The first function takes a matrix as input and 
##creates a list of four functions that are passed to the second function.
##The second function calculates the inverse of the inputted matrix from the first function.

#First Function is a direct analog to makeVector in the instructions.

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

#The following function takes the cached matrix as input and returns the inverse of 
#the matrix inputed into the makeCacheMatrix function above. Ultimately, the program will execute
#like the following:cachesolve(makeCacheMatrix(matrix(...))) or any logical equivalent.
#this is the analog to the cachemean in the instructions.

cachesolve <- function(x, ...) {
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