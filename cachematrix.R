## Put comments here that give an overall description of what your
## functions do 

##makeCacheMatrix func is to create the special matrix, readback/get 
##its value and cache its inverse, and also readback/get its inverse
##whereas cachesolve is to compute the inverse of the matrix conditionally,
##if the inverse has not been computed already, else the function
##should cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setinvMat <- function(inverse) invMatrix <<- inverse
  getinvMat <- function() invMatrix
  list(set = set, get = get,
       setinvMat = setinvMat,
       getinvMat = getinvMat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getinvMat()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setinvMat(invMatrix)
  invMatrix
  
}
