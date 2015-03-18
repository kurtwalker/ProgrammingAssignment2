## Programming Assignment 2.
## Together these functions allow computing the inverse of a square matrix and
## caching the value to avoid duplicate operations when more than one function 
## call is made.

## Takes a square matrix
## returns a list of functions that can cache the results of any function on the matrix
makeCacheMatrix <- function(the_matrix = matrix()) {
  cached_matrix <- NULL
  set <- function(new_matrix) {
    the_matrix <<- new_matrix
    cached_matrix <<- NULL
  }
  get <- function() the_matrix
  setinverse <- function(inverse) cached_matrix <<- inverse
  getinverse <- function() cached_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 

}


## Takes a special list of cache matrix functions.  See makeCacheMatrix
## returns the inverse of the cache matrix.
## Will use a cached value when previous computed to avoid duplicate operations.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
