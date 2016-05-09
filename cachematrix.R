## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of the matrix rather than compute it repeatedly.
## The example of its working is illustrated via two functions:-
## makeCacheMatrix and cacheSolve.

## This function will create and cache the inverse into a matrix object.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y){
    
    x<<- y
    m <<- NULL
    
  }
  
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
    
  }


## This function demonstrates how caching works by inversing the matrix passed. 
## If the matrix is cached, it uses the cached version, else does the calculation
## in the function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  
  if (!is.null(m)){
    
    message("getting cached data")
    return(m)
    
  }
  
  datamatrix <- x$get()
  
  m <- solve(datamatrix, ...)
  x$setInverse(m)
  m
  
}
