## ----------------------------------------------------------------------------
## This file contains functions `makeCacheMatrix` and `cacheSolve` 
## which could be used for caching inverse matrixes
## ----------------------------------------------------------------------------

## Function `makeCacheMatrix` creates a special "vector", which is
## a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ## `inv` - value of the inverse matrix for input matrix x
  ## It's not calculated yet so set its value = NULL
  inv <- NULL
  
  ## `set` function sets stored in the list matrix x = input matrix y
  set <- function(y) {
    x <<- y
    
    ## `inv` - value of the inverse matrix for input matrix x
    ## We set matrix x to the new value so we should unset `inv` value.
    ## It's not calculated yet (for the new matrix) so set its value = NULL
    inv <<- NULL
  }
  
  ## `get` function returns the value of the matrix stored in this list
  get <- function() x
  
  ## `setinverse` functions 
  setinverse <- function(solve) inv <<- solve
  
  ## `getinverse` function returns the value of the stored inverse matrix
  getinverse <- function() inv
  
  ## Return list containing set, get, setinverse, getinverse functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function `cacheSolve` returns an inverse matrix for the given vector created by 
## function `makeCacheMatrix`.
## If inverse matrix has already been calculated, function returns cached value.
## If inverse hasn't been calculated yet, function calculates it and saves for future use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Set inv variable = stored value of the inverse matrix
  inv <- x$getinverse()
  
  ## If inverse matrix has already been calculated
  if(!is.null(inv)) { 
    message("getting cached data")
  
    ## Return stored value
    return(inv)
  }
  
  ## If inverse matrix hasn't been calculated yet:
  ## 1. Get matrix from list
  data <- x$get()
  
  ## 2. Calculate inverse matrix for given matrix and save it to inv variable
  inv <- solve(data, ...)
  
  ## 3. Set inverse matrix value in the list x
  x$setinverse(inv)
  
  ## 4. Return value of the inverse matrix
  inv
}
