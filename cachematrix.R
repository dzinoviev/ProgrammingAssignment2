## The pair of functions facilitate the calculation of the inverse of a
## matrix by caching the most recent inversion result.
## The matrix is assumed to be invertable.

## The function creates a "smart matrix" object that can cache its inverse

makeCacheMatrix <- function (x = matrix ()) {
  # The actual cache
  inverted <- NULL
  
  # The "access" functions
  set <- function (y)  {
    # We refer to the parent environment, not to the locals!
    x <<- y
    inverted <<- NULL
  }
  get <- function () x
  set.inverted <- function (inv) inverted <<- inv
  get.inverted <- function () inverted
  
  # The list of the "access" functions
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
