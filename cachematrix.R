# Caching the Inverse of a Matrix

# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
# If the inverse has already been calculated
# then the cachesolve retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

# test the functions
# makeCacheMatrix function
my.matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
# get matrix
my.matrix$get()
# inverse haven't not been computed
my.matrix$getInverse()
# use cacheSolve function to compute inverse and save it in cache
cacheSolve(my.matrix)
# use "solve" check if it is correct
solve(my.matrix$get())
# execute cacheSolve again, it retrieve the cached inverse
cacheSolve(my.matrix)
# execute getInverse again it is not null anymore
my.matrix$getInverse()

# test the functions on a 4 x 4 matrix
my.matrix <- makeCacheMatrix(matrix(c(6,3,4,5,6,4,5,6,4,54,12,75,76,56,7,45), 4, 4))
my.matrix$get()
my.matrix$getInverse()
cacheSolve(my.matrix)
# verify if the inverse is correct
solve(my.matrix$get())
# getting inverse from cache
cacheSolve(my.matrix)
# retrieve inversed matrix from cache
my.matrix$getInverse()
