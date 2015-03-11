## Put comments here that give an overall description of what your
## functions do

## The function creates a "smart matrix" object that can cache its inverse

makeCacheMatrix <- function (x = matrix ()) {
  inverted <- NULL
  set <- function (y)  {
    x <<- y
    inverted <<- NULL
  }
  get <- function () x
  set.inverted <- function (inv) inverted <<- inv
  get.inverted <- function () inverted
  list (set = set, get = get, 
        set.inverted = set.inverted, 
        get.inverted = get.inverted)
}


## The function computes an inverse of the "smart matrix"

cacheSolve <- function (x, ...) {
  inverted <- x$get.inverted ()
  if (is.null (inverted)) { # The matrix has not been inverted recently!
    mtx <- x$get ()
    inverted <- solve (mtx, ...)
    x$set.inverted (inverted)
  }
  inverted
}

