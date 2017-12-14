## makeCacheMatrix creates a special "matrix" which
#sets the value of the matrix
#gets the value of the matrix
#sets the inverse value of the matrix
#gets the inverse value of the matrix

##NOTE: These functions assume that the matrix supplied is invertible

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse 
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## cacheSolve calculates the inverse of the matrix created above
#checks if the inverse is already calculated
#if it is calculated, it returns the inverse
#if it is not yet cached, it will calculate the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}