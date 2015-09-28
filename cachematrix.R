## Two functions "makeCacheMatrix" and "cacheSolve" below 
## cache the inverse of a matrix

## "makeCacheMatrix" function creates a special matrix object 
## that can cache its inverse (I)

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  I <- NULL
  ## Set the matrix
  set <- function( matrix ) {
    x <<- matrix
    I <<- NULL
  }
  ## Get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    I <<- inverse
  }
  ## Get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    I
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## "cacheSolve" function Compute the inverse of the special matrix 
## that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  ## If the inverse has already been calculated (the matrix has not changed), 
  ## the "cachesolve" should retrieve the inverse from the cache.        
  I <- x$getInverse()
  ## Return the inverse if its already calculated
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  ## Otherwise get the matrix from object
  data <- x$get()
  ## Calculate the inverse using matrix multiplication
  I <- solve(data, ...)
  ## Set the inverse to the object
  x$setInverse(I)
  
  ## Return the matrix
  I
}

