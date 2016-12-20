## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A list with set and get matrix along with set and get inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        matrix_inv <- NULL
        set <- function(y) {
            x <<- y
            matrix_inv <<- NULL
      }
      get <- function() x
      set_inverse <- function(solve) matrix_inv <<- solve
      get_inverse <- function() matrix_inv
      list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function
## Inverse of a matrix is returned which act as input for makeCacheMatrix() function. 
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      matrix_inv <- x$get_inverse()
      if(!is.null(matrix_inv)) {
            message("getting cached data")
            return(matrix_inv)
      }
      data <- x$get()
      matrix_inv <- solve(data, ...)
      x$set_inverse(matrix_inv)
      matrix_inv
}
