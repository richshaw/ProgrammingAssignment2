## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix", which is really a list containing a function which
## 0. Checks we have a square matrix
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## Validation you can only inverse a square matrix using solve()
  ## Check we have a square matrix
  if(nrow(x) != ncol(x)) {
    stop('Matrix isn\'t square. Inverse can only be calculated on square matrix')
  }
  
  #On creation initalize value of inverse var
  i <- NULL
  ## Save matrix data in object
  ## Clears any previously calculated inverse
  set <- function(y) {
    x <<- y
    i <<- NULL 
  }
  ## Returns cached matrix data
  get <- function() x
  ## Cache inverse of matrix
  setInverse <- function(inverse) i <<- inverse
  ## Return cache inverse of matrix
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## Function calculates the inverse of the special "matrix" created with the makeCacheMatrix function. 
## 'x' is a makeCacheMatrix object
## It first checks to see if the inverse has already been calculated. 
## If it has gets it the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of 'x' 
## and sets the value of the inverse matrix in the cache using the setinverse function.
cacheSolve <- function(x, ...) {
  ## Looks for inverse in cache
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    ## Inverse is not null so return value
    return(i)
  }
  ## Inverse is not availble so calculate
  ## Get matrix data from the makeCacheMatrix object
  data <- x$get()
  ## Calculate inverse of data
  i <- solve(data, ...)
  ## Cache inverse in object
  x$setInverse(i)
  ## Return inverse of matrix
  i
}
