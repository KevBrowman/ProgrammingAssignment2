## calculate the inverse of a matrix.

## This function creates a list of functions that helps out in the exercise of caching the inverse of a matrix. 
## It receive as an input a matrix that have to be an invertible matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(invers) m <<- invers
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function calculates the inverse of a matrix if it has not already been calculated, otherwise returns
## the already calculated matrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
}

ma <- matrix(c(2,2,3,-2,1,-2,2,0,2),nrow = 3,ncol = 3)
z <- makeCacheMatrix(ma)
cacheSolve(z)
z$get()

##      [,1] [,2] [,3]
##[1,]    2   -2    2
##[2,]    2    1    0
##[3,]    3   -2    2

z$getInverse()
##      [,1] [,2] [,3]
##[1,] -1.0    0    1
##[2,]  2.0    1   -2
##[3,]  3.5    1   -3


ma2 <- matrix(c(2,2,3,2),nrow=2,ncol=2)

z$set(ma2)
z$get()
##    [,1] [,2]
##[1,]    2    3
##[2,]    2    2

cacheSolve(z)
z$getInverse()
##      [,1] [,2]
##[1,]   -1  1.5
##[2,]    1 -1.0


