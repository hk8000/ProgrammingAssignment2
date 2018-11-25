
## Caching the Inverse of a Matrix:
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

# This function gets a matrix as input and does the following
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  
  # set the value of the matrix
  setmatrix <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  
  getmatrix <- function() x						# get the matrix value
  setinverse <- function(inverse) invmatrix <<- inverse                 # set the invertible matrix value
  getinverse <- function() invmatrix  			                # get the invertible matrix value
  list(setmatrix=setmatrix, 
       getmatrix=getmatrix, 
       setinverse=setinverse, 
       getinverse=getinverse)

}

# This function get the output of makeCacheMatrix  and returns the inverse of the matrix.
# If the inverse already exists n the cache, it will return it, skippping recalculating it.
# if not cached, then it calculated the inverse matrix returns it
# It is assumed the inverse is always invertable

cacheSolve <- function(x, ...) {
  
  #get the inverse value of the matrix from makeCacheMatrix if it exists
  
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {							# if inverse matrix is not null
    message("getting cached inverset matrix.")
    return(invmatrix)
  }
  #if no inverse matrix does not exists, calculate it
  data <- x$getmatrix()
  invmatrix <- solve(data)
  x$setinverse(invmatrix)
  invmatrix
}
        
