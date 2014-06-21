## The following two functions cache the inverse of a matrix. Matrix inversion can often be a costly procedure. By placing previously calculated values in the cache they are able to be retrieved (a less costly process).

## makeCacheMatrix makes a list containing a function to;
1- set the value of the matrix
2- get the value of the matrix
3- set the value of the inverse matrix
4- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function returns the inverse of the matrix. Firstly it checks if the inverse has already been calculated. If it has it gets the result and skips the calculation. If not it calculates the inverse and sets this value in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
