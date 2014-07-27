## Put comments here that give an overall description of what your
## functions do


#This function creates a special "matrix" object that can cache its inverse. The solve(x) function computes the
# inverse of a square matrix X
# Here the assumption is that the input matrix is a square matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL    
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# This function first tests if the inverse has already been calculated (and the matrix has not changed), if yes, 
# it would use the same matrix, if not it would go on to compute the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
