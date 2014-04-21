## Put comments here that give an overall description of what your
## functions do
# You first feed makeCacheMatrix() with the matrix that you are about to invert, and assign the returned object 
# to a variable, and then use cacheSolve() with this variable as the argument to invert the matrix.

## Write a short comment describing this function
# it will create and keep a local environment to store both the matrix you provide and its inverse (using set_inv()).
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## Write a short comment describing this function
# it will first try to see if the inverse has already been calculated in the local environment created by makeCacheMatrix(),
# if true return the cached solution; if not, compute the solution, cache it and return it.
cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}
