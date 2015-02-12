## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix", which is really a 
## list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize inverse matrix variable
  inv <- NULL
  
  # set: update matrix und reset inverse matrix variable
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  
  # get: get the matrix
  get <- function() x
  
  # setinv: set the inverse of the matrix
  setinv <- function(inverse) inv <<- inverse
  
  # getinv: get the inverse of the matrix
  getinv <- function() inv
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## cachSolve: This function calculates the inverse of the "matrix" created with
## the above funkction. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the matrix and 
## updates the value of the cache.

cacheSolve <- function(x, ...) {
  
  # load value from cache
  inv <- x$getinv()
  
  if(!is.null(inv)){
    message("getting cached inverse of the matrix")
    return(inv)
  }
  
  # get matrix 
  data <- x$get()
  # calculate inverse of the matrix
  inv <- solve(x, ...)
  # update cache
  x$setinv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
