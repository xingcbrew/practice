## This is a practice script to learn git! 
##Authors : Xing, Jostein, Babu

## First project: This is a function to cache a inverse of a matrix;
makeCacheMatrix <- function(n = matrix()) {
  p <- NULL
  set <- function(r) {
    n <<- r
    p <<- NULL
  }
  get <- function() n
  setinverse <- function(inverse) n <<- inverse
  getinverse <- function() p
  list(set = set,
       +        get = get,
       +        setinverse = setinverse,
       +        getinverse = getinverse)
}
> cacheSolve <- function(n, ...) {
  p <- n$getinverse()
  if (!is.null(p)) {
    message("getting cached data")
    return(p)
  }
  data <- n$get()
  p <- solve(data, ...)
  n$setinverse(p)
  p
}

## Testing the function;

M <- matrix(c(5,9,-6,12),2,2)
#
K <- makeCacheMatrix(M)

cacheSolve(K)