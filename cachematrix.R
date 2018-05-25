## Two functions to cachae the inverse of an inversible matrix


## makeCacheMatrix creates a matrix that can cache its inverse

makeCacheMatrix <- function(matrix = matrix()) {
  
  matrix_inverse <- NULL
  set <- function(y) {
    matrix <<- y
    matrix_inverse <<- NULL
  }
  
  get <- function() matrix
  set_inverse <- function(inv) matrix_inverse <<- inv
  get_inverse <- function() matrix_inverse
  
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve retrieves the inverse 
## from the cache

cacheSolve <- function(matrix, ...) {
  
  matrix_inverse <- matrix$get_inverse()
  
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  
  data <- matrix$get()
  matrix_inverse <- solve(data, ...)
  matrix$set_inverse(matrix_inverse)
  matrix_inverse
}