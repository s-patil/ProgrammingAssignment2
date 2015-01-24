## makeCacheMatrix makes a vector of four functions that can set a matrix, get a matrix,
## set inverse of the matrix and get inverse of the matrix. CacheSolve will calculate the inverse of a matrix

## This function will return a list of four functions

makeCacheMatrix <- function(x = matrix()) {
  
  I<- NULL
  setMatrix <- function(y) {
    if(!identical(x,y)){   
    x <<- y
    I <<- NULL
    }
  }
  getMatrix <- function() x
  setInverse <- function(Inverse) I <<- Inverse
  getInverse <- function() I
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## This function will calculate the inverse of a matrix if it's not cached

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$getMatrix()
  I <- solve(data, ...)
  x$setInverse(I)
  I
}
