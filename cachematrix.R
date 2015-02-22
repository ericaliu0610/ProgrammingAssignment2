## functions that cache the inverse of a matrix.

#The first function, makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
#which is really a list containing a function to 
#1 "set": set the value of the matrix
#2 "get": get the value of the matrix
#3 "setinverse": set the value of the its inverse
#4 "getinverse":get the value of the its inverse
#"x_inverse" represents the reverse of matrix "x".
makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) x_inverse <<- solve
  getinverse <- function() x_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ##generate a list of matrix "x"
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        
  x_inverse <- x$getinverse()
  if(!is.null(x_inverse)) {
    message("getting cached data")
    return(x_inverse)
  }
  data <- x$get()
  x_inverse <- solve(data, ...)
  x$setinverse(x_inverse)
  x_inverse
  ## Return a matrix that is the inverse of 'x'
}
