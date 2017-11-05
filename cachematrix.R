## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  SetMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  GetMatrix <- function() { x }
  SetInverse <- function(inverse) { inv <<- inverse }
  GetInverse <- function() { inv }
  list(SetMatrix = SetMatrix, GetMatrix = GetMatrix, SetInverse = SetInverse, GetInverse = GetInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$GetInverse()
  if(!is.null(inv))
  {
    message("Getting Cache Invertible Matrix")
    return(inv)
  }
  MatrixData <- x$GetMatrix()
  inv <- solve(MatrixData,...)
  x$SetInverse(inv)
  return(inv)
}

