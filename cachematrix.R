## This R script encapsulates all the functionality to creat a special matrix 
## which knows how to calculate its inverse and has the ability to cache the same to
## to prevent costly and time consuming matrix inverse operation

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Default the matrix inverse object to NULL
  m <- NULL
  ## This function is important to ensure if matrix undergoes a change then 
  ## the variable which stores the inverse is reset to NULL so that it can be re-computed else
  ## getinverse function may return an incorrect result
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## This function returns the actual matrix
  get <- function() x
  ## This function sets the matrix inverse value
  setinverse <- function(inverse) m <<- inverse
  ## This function retrives the matrix inverse value
  getinverse <- function() m
  ## vector to maintain a list of functions to set/get the actual matrix and its inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## First get the inverse of the given matrix if exists
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix result")
    return(m)
  }
  ## Retrieve the actual matrix
  data <- x$get()
  ## If the inverse of the given matrix doesn't exist - find one
  m <- solve(data, ...)
  ## Now store the matrix inverse back into the cache
  x$setinverse(m)
  ## Return the matrix inverse
  m
}

