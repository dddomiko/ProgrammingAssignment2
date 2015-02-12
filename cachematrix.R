## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix", which is really a 
## list containing a function to

## set: set the value of the matrix
## get: get the value of the matrix
## setinv: set the inverse of the matrix
## getinv: get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize inverse matrix variable
  inv <- NULL
  
  # set: update matrix und reset inverse matrix variable
  set <- function(y){
    x <<- y
    inv <<- NULL
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


## cacheSolve: This function calculates the inverse of the "matrix" created with
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
  inv <- solve(data, ...)
  # update cache
  x$setinv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}

### testing ####################################################################

## initialize "special" matrix
# test <- makeCacheMatrix(matrix(c(2,3,4,5),nrow = 2))

## get matrix
# test$get()
#
# >      [,1] [,2]
# > [1,]    2    4
# > [2,]    3    5

## get inverse of the matrix
# test$getinv()
# 
# > NULL

## calculate inverse matrix
# cacheSolve(test)
# 
# >      [,1] [,2]
# > [1,] -2.5    2
# > [2,]  1.5   -1

## calculate inverse matrix again
# cacheSolve(test)
# 
# > getting cached inverse of the matrix
# >      [,1] [,2]
# > [1,] -2.5    2
# > [2,]  1.5   -1

## get inverse of the matrix
# test$getinv()
# 
# >      [,1] [,2]
# > [1,] -2.5    2
# > [2,]  1.5   -1

## set new matrix
# test$set(matrix(c(3,3,5,6),nrow = 2))

## check: cache deleted
# test$getinv()
# 
# > NULL