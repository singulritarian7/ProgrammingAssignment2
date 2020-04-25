## Put comments here that give an overall description of what your functions do.

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## My assignment is to write a pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function

## This function, "makeCacheMatrix", creates a special matrix, which is really a list containing a function to:

# 1-set the value of the matrix
# 2-get the value of the matrix
# 3-set the value of the inverse matrix
# 4-get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function.

## This function "cacheSolve" will take the object (list of functions) returned by makeCacheMatrix().
## First, it calls getinverse() and if i is not NULL,it will return i. If i is NULL, it will call 
## get() to get the matrix stored in the object, then an inverse matrix is calculated and stored 
## back by calling setinverse() and then return the inverse matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("cached inverse matrix found, getting the matrix...")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
