## My functions aim to get and cache the inverse matrix

## This function is to help cache the inverse matrix so as to save time from calculation

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function (y) {
    x <<- y
    invM <<- NULL
  }
  get <- function () x
  setInverse <- function (solveMatrix) invM <<- solveMatrix
  getInverse <- function () invM
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invM <- x$getInverse()
  if(!is.null(invM)) {
    message('getting cached data')
    return(invM)
  }
  data <- x$get
  invM <- x$solve(data)
  x$setInverse
}
