## This creates a pair of functions that cache the inverse of a matrix

## This first one creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y ## Setting the value of the matrix
    i <<- NULL
  }
  get <- function() x ## Getting the value of the matrix
  setinverse <- function(inverse) i <<- inverse ## Setting the inverse
  getinverse <- function() i ## Getting the value of the inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This last one computes the inverse of the special "matrix" returned by
## the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) { ## Checking if the inverse has already been calculated
    message ("getting cached data")
    return(i) ## The function retrieves the inverse from the cache
  }
  data <- x$get()
  i <- solve(data, ...) ## Otherwise it calculates the inverse of the data
  x$setinverse(i) ## and sets the value of the inverse in the cache 
  i
}
