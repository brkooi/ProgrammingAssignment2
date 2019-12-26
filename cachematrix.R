# cachematrix.R
# 
# This script contains 4 functions:
#   1. makeVector;
#   2. cachemean;
#   3. makeCacheMatrix;
#   4. cacheSolve.
# 
# makeVector and cachemean is created as an exercise for understanding the final assignment makeCacheMatrix.
# These examples were posted by Leonard Greski at
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md


## makeVector
## Creates an instance a vector object.
## The object can be mutated by set and setmean
## The object can be accessed by get and getmean
makeVector <- function(x = numeric()) {
    m <- NULL
    
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## cachemean
## Returns the mean of the instance of the vector object.
## The object can be mutated by set and setInverse
## The object can be accessed by get and getInverse
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}


## makeCacheMatrix
## Creates an instance of a matrix object
## The object can be mutated by set and setInverse
## The object can be accessed by get and getInverse

makeCacheMatrix<-function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,getInverse = getInverse)
}


## cacheSolve
## Returns an instance of a matrix object that is the inverse of the passed instance of a matrix object.
## It only works with a square matrix, else Error in solve.default(data, ...) : 'a' (R x C) must be square
## If the matrix is not invertible this error occurs:
# system is computationally singular: reciprocal condition number = 1.33888e-17

cacheSolve <- function(x, ...) {
       
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) # ?solve for explanation of solve(). Found solution on stackoverflow
  x$setInverse(m)
  m
}
