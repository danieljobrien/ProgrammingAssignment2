## Cache the inverse of a matrix


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  solvemat <- function(solve) m <<- solve
  getmat <- function() m
  list(set = set, get = get,
       solvemat = solvemat,
       getmat = getmat)

}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$solvemat(m)
  m
  
}
